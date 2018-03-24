{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Azure.Storage.Blob
    ( BlobClient
    , getBlobClient
    , ContainerName
    , containerName_
    , BlobName
    , blobName_
    , Error(..)
    , createContainer
    , createContainer'
    , CreateContainerOptions(..)
    , defaultCreateContainerOptions
    , ContainerAccess(..)
    , deleteContainer
    , listContainers
    , listBlobs
    , putBlob
    , Marker
    , listContainersFromMarker
    , BlobType(..)
    )
where

import Azure.Storage.Authentication

import           Control.Monad.Except (MonadError, throwError, liftIO, MonadIO)
import           Control.Monad.Trans.Resource (MonadResource, MonadThrow)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit ((.|), runConduit, yield, ConduitT, fuseUpstream, Void)
import qualified Data.Conduit.Combinators as CC
import           Data.Default.Class (def)
import           Data.Semigroup ((<>))
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Conduit (http, responseBody)
import           Network.HTTP.Types (renderQuery)
import           Network.HTTP.Types.Header (hContentLength)
import           Network.HTTP.Types.Method (methodPut, methodDelete)
import           Network.HTTP.Types.QueryLike (toQuery)
import qualified Network.HTTP.Types.Status as S
import qualified Text.XML.Stream.Parse as X
import           Data.String (IsString)

data BlobClient = BlobClient
    { blobReq :: HTTP.Request
    , blobCreds :: Credentials
    , blobHttp :: HTTP.Manager
    }

getBlobClient :: StorageAccount -> HTTP.Manager -> Maybe BlobClient
getBlobClient StorageAccount{credentials, blobEndpoint} manager =
    -- TODO Send @snoyberg upstream a `requestFromURI{,_}`
    clientFromRequest <$> (HTTP.parseRequest =<< show <$> blobEndpoint)
    where
    clientFromRequest request = BlobClient (setRequestDefaults request) credentials manager
    setRequestDefaults r = r { HTTP.requestHeaders = [("x-ms-version", apiVersion)] }

-- | A container name is a valid DNS name.
newtype ContainerName = ContainerName Text
    deriving (Show, Eq, Ord)

containerName_ :: Text -> ContainerName
containerName_ = ContainerName

newtype BlobName = BlobName Text
    deriving (Show, Eq, Ord)

blobName_ :: Text -> BlobName
blobName_ = BlobName

issueRequest :: MonadResource m => BlobClient -> HTTP.Request -> ConduitT ByteString Void m b -> m b
issueRequest BlobClient{blobCreds, blobHttp} rawRequest f = do
    request <- signRequest blobCreds rawRequest
    response <- http request blobHttp
    runConduit (responseBody response .| f)

data Error
    = HttpError (HTTP.Response LBS.ByteString)
    | AzureError ByteString (HTTP.Response LBS.ByteString)
    deriving (Eq, Show)

issueRequestNoBody :: (MonadIO m, MonadError Error m) => BlobClient -> (HTTP.Request -> HTTP.Request) -> (S.Status -> Maybe b) -> m b
issueRequestNoBody BlobClient{blobCreds, blobHttp, blobReq} mkreq handler = do
    request <- signRequest blobCreds (mkreq blobReq)
    response <- liftIO (HTTP.httpLbs request blobHttp)
    case handler (HTTP.responseStatus response) of
        Just r -> return r
        Nothing ->
            case lookup "x-ms-error-code" (HTTP.responseHeaders response) of
                Just errorCode -> throwError (AzureError errorCode response)
                Nothing -> throwError (HttpError response)

listContainers :: (MonadResource m, MonadThrow m) => BlobClient -> ConduitT () ContainerName m ()
listContainers bc = go Text.empty
    where
    go marker =
        runConduit (listContainersFromMarker bc marker `fuseUpstream` CC.mapM_ yield)
        >>= \case
            Nothing -> return ()
            Just newMarker -> go newMarker

type Marker = Text
listContainersFromMarker :: (MonadResource m, MonadThrow m) => BlobClient -> Marker -> ConduitT () ContainerName m (Maybe Marker)
listContainersFromMarker bc@BlobClient{blobReq} marker = do
    issueRequest bc
        blobReq { HTTP.queryString = renderQuery True (toQuery query) }
        (X.parseBytes def .| enumerationResults `fuseUpstream` CC.mapM_ yield)

    where
    query :: [(Text, Text)]
    query
        | Text.null marker = defQuery
        | otherwise = ("marker", marker) : defQuery
        where defQuery = [("comp", "list")]

    enumerationResults = X.force "expected EnumerationResults" $
        X.tagIgnoreAttrs "EnumerationResults" $ do
            _ <- X.ignoreTreeContent "Prefix"
            _ <- X.ignoreTreeContent "Marker"
            _ <- X.ignoreTreeContent "MaxResults"

            X.force "expected Containers" $ X.tagNoAttr "Containers" $
                X.manyYield $ X.tagNoAttr "Container" $ do
                    name <- X.force "expected Container/Name" $ X.tagNoAttr "Name" X.content
                    _ <- X.ignoreTreeContent "Properties"
                    return (ContainerName name)

            X.force "expected NextMarker" $
                X.tagNoAttr "NextMarker" $ do
                    nextMarker <- X.content
                    return (if Text.null nextMarker then Nothing else Just marker)

listBlobs :: (MonadResource m, MonadThrow m) => BlobClient -> ContainerName -> ConduitT () BlobName m ()
listBlobs bc cn = go Text.empty
    where
    go marker =
        runConduit (listBlobsFromMarker bc cn marker `fuseUpstream` CC.mapM_ yield)
        >>= \case
            Nothing -> return ()
            Just newMarker -> go newMarker

listBlobsFromMarker :: (MonadResource m, MonadThrow m) => BlobClient -> ContainerName -> Marker -> ConduitT () BlobName m (Maybe Marker)
listBlobsFromMarker bc@BlobClient{blobReq} (ContainerName cn) marker = do
    issueRequest bc
        blobReq {
            HTTP.queryString = renderQuery True (toQuery query),
            HTTP.path = HTTP.path blobReq <> "/" <> encodeUtf8 cn -- name is safe without encoding
        }
        (X.parseBytes def .| enumerationResults `fuseUpstream` CC.mapM_ yield)

    where
    query :: [(Text, Text)]
    query
        | Text.null marker = defQuery
        | otherwise = ("marker", marker) : defQuery
        where defQuery = [("comp", "list"), ("restype", "container")]

    enumerationResults = X.force "expected EnumerationResults" $
        X.tagIgnoreAttrs "EnumerationResults" $ do
            _ <- X.ignoreTreeContent "Prefix"
            _ <- X.ignoreTreeContent "Marker"
            _ <- X.ignoreTreeContent "MaxResults"
            _ <- X.ignoreTreeContent "Delimiter"

            X.force "expected Blobs" $ X.tagNoAttr "Blobs" $
                X.manyYield $ X.tagNoAttr "Blob" $ do
                    name <- X.force "expected Blob/Name" $ X.tagNoAttr "Name" X.content
                    _ <- X.ignoreTreeContent "Deleted"
                    _ <- X.ignoreTreeContent "Snapshot"
                    _ <- X.ignoreTreeContent "Properties"
                    _ <- X.ignoreTreeContent "Metadata"
                    return (BlobName name)

            X.force "expected NextMarker" $
                X.tagNoAttr "NextMarker" $ do
                    nextMarker <- X.content
                    return (if Text.null nextMarker then Nothing else Just marker)

data ContainerAccess
    = ContainerPublic
    | BlobsPublic
    | Private
    deriving (Eq, Show)

displayContainerAccess
    :: IsString s
    => ContainerAccess -> s
displayContainerAccess ContainerPublic = "container"
displayContainerAccess BlobsPublic = "blob"
displayContainerAccess Private = "private"

data CreateContainerOptions = CreateContainerOptions { containerAccess :: ContainerAccess }

defaultCreateContainerOptions :: CreateContainerOptions
defaultCreateContainerOptions = CreateContainerOptions Private

-- | Tries to create a container with the given name.
--
-- Returns @True@ if the container was created, or @False@ if the container
-- already existed.
createContainer :: (MonadIO m, MonadError Error m) => BlobClient -> ContainerName -> m Bool
createContainer bc cn = createContainer' bc cn defaultCreateContainerOptions

-- | Tries to create a container with the given name, and the given options.
--
-- Returns @True@ if the container was created, or @False@ if the container
-- already existed.
createContainer' :: (MonadIO m, MonadError Error m) => BlobClient -> ContainerName -> CreateContainerOptions -> m Bool
createContainer' bc (ContainerName name) opts =
    issueRequestNoBody bc req handle
    where
    req r = r {
        HTTP.method = methodPut,
        HTTP.path = HTTP.path r <> "/" <> encodeUtf8 name, -- name is safe without encoding
        HTTP.queryString = "?restype=container",
        HTTP.requestHeaders =
            case containerAccess opts of
                Private -> HTTP.requestHeaders r
                val ->
                    ("x-ms-blob-public-access", displayContainerAccess val) : HTTP.requestHeaders r
    }
    handle s
        | s == S.created201 = Just True
        | s == S.conflict409 = Just False
        | otherwise = Nothing

-- | Tries to delete a container with the given name.
--
-- Returns @True@ if the container scheduled for deletion, or @False@ if the
-- container was already deleted.
deleteContainer :: (MonadIO m, MonadError Error m) => BlobClient -> ContainerName -> m Bool
deleteContainer bc (ContainerName name) =
    issueRequestNoBody bc req handle
    where
    req r = r {
        HTTP.method = methodDelete,
        HTTP.path = HTTP.path r <> "/" <> encodeUtf8 name, -- name is safe without encoding
        HTTP.queryString = "?restype=container"
    }
    handle s
        | s == S.accepted202 = Just True
        | s == S.notFound404 = Just False
        | otherwise = Nothing

data BlobType
    = BlockBlob
    -- TODO
    -- PageBlob
    -- AppendBlob

putBlob :: (MonadIO m, MonadError Error m) => BlobClient -> ContainerName -> BlobName -> ByteString -> m ()
putBlob bc (ContainerName cn) (BlobName bn) bytes =
    issueRequestNoBody bc req handle
    where
    req r = r {
        HTTP.method = methodPut,
        HTTP.path = HTTP.path r <> "/" <> encodeUtf8 cn <> "/" <> encodeUtf8 bn, -- cn is safe, TODO about bn
        HTTP.requestHeaders =
            (hContentLength, BS8.pack (show (BS8.length bytes))) :
            ("x-ms-blob-type", "BlockBlob") : -- TODO
            HTTP.requestHeaders r,
        HTTP.requestBody = HTTP.RequestBodyBS bytes
    }
    handle s
        | s == S.created201 = Just ()
        | otherwise = Nothing
