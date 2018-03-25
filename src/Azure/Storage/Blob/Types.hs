{-# LANGUAGE OverloadedStrings #-}

module Azure.Storage.Blob.Types where

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Network.HTTP.Client as HTTP
import qualified Azure.Storage.Authentication as Auth

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

-- | User data associated to a Blob.
type Metadata = Map.Map Text Text

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
