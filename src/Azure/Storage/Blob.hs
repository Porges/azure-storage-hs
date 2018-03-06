{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Azure.Storage.Blob
    ( BlobClient
    , getBlobClient
    , ContainerName
    , containerName_
    , createContainer
    , deleteContainer
    , listContainers
    , Marker
    , listContainersFromMarker
    )
where

import Azure.Storage.Authentication

import           Control.Monad.Except (MonadError, throwError, liftIO, MonadIO)
import           Control.Monad.Trans.Resource (MonadResource, MonadThrow)
import           Data.ByteString (ByteString)
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
import           Network.HTTP.Types.Method (methodGet, methodPut, methodDelete)
import           Network.HTTP.Types.QueryLike (toQuery)
import qualified Network.HTTP.Types.Status as S
import qualified Text.XML.Stream.Parse as X

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

issueRequest :: MonadResource m => BlobClient -> HTTP.Request -> ConduitT ByteString Void m b -> m b
issueRequest BlobClient{blobCreds, blobHttp} rawRequest f = do
    request <- signRequest blobCreds rawRequest
    response <- http request blobHttp
    runConduit (responseBody response .| f)

data Error
    = HttpError (HTTP.Response ())
    | AzureError ByteString
    deriving (Eq, Show)

issueRequestNoBody :: (MonadIO m, MonadError Error m) => BlobClient -> (HTTP.Request -> HTTP.Request) -> (S.Status -> Maybe b) -> m b
issueRequestNoBody BlobClient{blobCreds, blobHttp, blobReq} mkreq handler = do
    request <- signRequest blobCreds (mkreq blobReq)
    response <- liftIO (HTTP.httpNoBody request blobHttp)
    case handler (HTTP.responseStatus response) of
        Just r -> return r
        Nothing ->
            case lookup "x-ms-error-code" (HTTP.responseHeaders response) of
                Just errorCode -> throwError (AzureError errorCode)
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
    let query =
            toQuery ((if Text.null marker then id else (("marker", marker):))
                [("comp" :: Text, "list" :: Text)])

    issueRequest bc
        blobReq { HTTP.queryString = renderQuery True query }
        (X.parseBytes def .| enumerationResults `fuseUpstream` CC.mapM_ yield)

    where

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

data ContainerPublicAccess
    = Container
    | Blob

-- | Tries to create a container with the given name.
--
-- Returns @True@ if the container was created, or @False@ if the container
-- already existed.
createContainer :: (MonadIO m, MonadError Error m) => BlobClient -> ContainerName -> m Bool
createContainer bc (ContainerName name) =
    issueRequestNoBody bc req handle
    where
        req r = r {
            HTTP.method = methodPut,
            HTTP.path = HTTP.path r <> "/" <> encodeUtf8 name, -- name is safe without encoding
            HTTP.queryString = "?restype=container"
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
