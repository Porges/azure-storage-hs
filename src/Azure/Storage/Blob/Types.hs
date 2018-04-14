{-# LANGUAGE OverloadedStrings #-}

module Azure.Storage.Blob.Types where

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import qualified Azure.Storage.Authentication as Auth
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Azure.Storage.Types as Types
import qualified Data.Maybe as Maybe
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types.Header as Header

-- | Binary data can be stored in one of the following types:
data BlobType
  = BlockBlob
  -- ^ optimized for streaming
  | AppendBlob
  -- ^ optimized for append operations
  | PageBlob
  -- ^ optimized for random read/write operations
  -- provide the ability to write to a range of bytes in a blob
  deriving Show
instance Types.FromBinary BlobType where
  parseBinary "BlockBlob" = pure BlockBlob
  parseBinary "AppendBlob" = pure AppendBlob
  parseBinary "PageBlob" = pure PageBlob
  parseBinary _ = Left "BlobType (BlockBlob|AppendBlob|PageBlob)"
instance Types.ToBinary BlobType where
  toBinary = BSC.pack . show

-- | User data associated to a Blob.
type Metadata = Map.Map Text Text

decodeMetadata :: Header.RequestHeaders -> Metadata
decodeMetadata
  = Map.map TE.decodeUtf8
  . Map.mapKeys TE.decodeUtf8
  . Map.fromList
  . map (\(k,v) -> (Maybe.fromJust k, v) )
  . filter (\h -> Maybe.isJust . fst $ h)
  . map (\(k,v) -> (BS.stripPrefix "x-ms-meta-" k, v))
  . map (\(k,v) -> (CI.original k, v) )

encodeMetadata :: Metadata -> Header.RequestHeaders
encodeMetadata
  = Map.toList
  . Map.mapKeys (CI.mk . BS.append "x-ms-meta-" . TE.encodeUtf8)
  . Map.map TE.encodeUtf8

-- | A container name is a valid DNS name.
newtype ContainerName = ContainerName { unContainerName :: Text }
  deriving (Show, Eq, Ord)

-- | A blob name is a valid DNS name.
newtype BlobName = BlobName { unBlobName :: Text }
  deriving (Show, Eq, Ord)

data Client = Client
  { blobReq :: HTTP.Request
  , blobCreds :: Auth.Credentials
  , blobHttp :: HTTP.Manager
  }

getClient :: Auth.StorageAccount -> HTTP.Manager -> Maybe Client
getClient account manager
  = -- TODO Send @snoyberg upstream a `requestFromURI{,_}`
    clientFromRequest <$> (HTTP.parseRequest =<< show <$> blobEndpoint)
  where
    credentials = Auth.credentials account
    blobEndpoint = Auth.blobEndpoint account
    clientFromRequest request = Client (setRequestDefaults request) credentials manager
    setRequestDefaults r = r { HTTP.requestHeaders = [("x-ms-version", Auth.apiVersion)] }
