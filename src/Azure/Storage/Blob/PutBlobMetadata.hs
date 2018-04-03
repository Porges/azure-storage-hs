{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The <https://docs.microsoft.com/en-us/rest/api/storageservices/set-blob-metadata Put Blob Metadata>
-- operation sets user-defined metadata for the specified blob as one or more name-value pairs.
module Azure.Storage.Blob.PutBlobMetadata (
  -- * Creating a Request
    createPutBlobMetadata
  , PutBlobMetadata

  -- * Request Lenses
  , pbmContainerName
  , pbmBlobName
  , pbmTimeout
  , pbmMetadata
  , pbmLeaseId
  , pbmClientRequestId

  -- * Destructuring the Response
  , createPutBlobMetadataResponse
  , PutBlobMetadataResponse

  -- * Response Lenses
  , pbmrETag
  , pbmrLastModified
  , pbmrRequestId
  , pbmrServerEncrypted
) where

import qualified Azure.Storage.Request as Request
import qualified Azure.Storage.Blob.Types as Blob
import qualified Azure.Storage.Types as Types
import           Data.Monoid ((<>))
import           Lens.Micro ((^.), Lens', lens)
import           Data.Text (Text)
import           Numeric.Natural (Natural)
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Conduit as HTTPConduit
import qualified Data.Time as Time
import qualified Data.Either as Either

--------------------------------------------------------------------------------
-- * Creating a Request

-- | Creates a value of 'PutBlobMetadata' with the minimal fields set.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbmContainerName' - Container where the blob should be put
--
-- * 'pbmBlobName' - Name of the blob
--
-- * 'pbmTimeout' - The timeout parameter is expressed in seconds.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/setting-timeouts-for-blob-service-operations Learn more>
--
-- * 'pbmMetadata' - Name-value pairs associated with the blob as metadata.
--
-- * 'pbmLeaseId' - Required if the blob has an active lease.
--
-- * 'pbmClientRequestId' - Provides a client-generated, opaque value with a 1 KB character limit that is recorded in the analytics logs.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/about-storage-analytics-logging Learn more>
--
createPutBlobMetadata
  :: Blob.ContainerName -- ^ 'pbmContainerName'
  -> Blob.BlobName      -- ^ 'pbmBlobName'
  -> Blob.Metadata      -- ^ 'pbmMetadata'
  -> PutBlobMetadata
createPutBlobMetadata cn bn md
  = PutBlob cn bn Nothing md Nothing Nothing

-- | /See:/ 'createPutBlobMetadata' smart constructor.
data PutBlobMetadata = PutBlob
  { _pbmContainerName :: Blob.ContainerName
  , _pbmBlobName :: Blob.BlobName
  , _pbmTimeout :: Maybe Natural
  , _pbmMetadata :: Blob.Metadata
  , _pbmLeaseId :: Maybe Text
  , _pbmClientRequestId :: Maybe Text
  } deriving Show

instance Request.ToRequest PutBlobMetadata where
  toMethod = const Method.methodPut
  toHeaders o = staticHeaders
             <> Blob.encodeMetadata (o ^. pbmMetadata)
    where
      staticHeaders
        = Request.mkBinaryPairs
        [ ("x-ms-lease-id", o ^. pbmLeaseId)
        , ("x-ms-client-request-id", o ^. pbmClientRequestId)
        ]
  toQuery o
    = Map.fromList
    $ ("comp", "metadata")
    : Request.mkBinaryPairs [ ("timeout", o ^. pbmTimeout) ]
  toPath o
    = (TE.encodeUtf8 . Blob.unContainerName $ o ^. pbmContainerName)
    <> "/"
    <> (TE.encodeUtf8 . Blob.unBlobName $ o ^. pbmBlobName)
  type Rs PutBlobMetadata = PutBlobMetadataResponse
  parseResponse = const parseResponse

--------------------------------------------------------------------------------
-- * Request Lenses

-- | Container where the blob should be put
pbmContainerName :: Lens' PutBlobMetadata Blob.ContainerName
pbmContainerName = lens _pbmContainerName (\ s a -> s{_pbmContainerName = a})

-- | Name of the blob
pbmBlobName :: Lens' PutBlobMetadata Blob.BlobName
pbmBlobName = lens _pbmBlobName (\ s a -> s{_pbmBlobName = a})

-- | The timeout parameter is expressed in seconds.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/setting-timeouts-for-blob-service-operations Learn more>
pbmTimeout :: Lens' PutBlobMetadata (Maybe Natural)
pbmTimeout = lens _pbmTimeout (\ s a -> s{_pbmTimeout = a})

-- | Name-value pairs associated with the blob as metadata.
pbmMetadata :: Lens' PutBlobMetadata Blob.Metadata
pbmMetadata = lens _pbmMetadata (\ s a -> s{_pbmMetadata = a})

-- | Required if the blob has an active lease.
pbmLeaseId :: Lens' PutBlobMetadata (Maybe Text)
pbmLeaseId = lens _pbmLeaseId (\ s a -> s{_pbmLeaseId = a})

-- | Provides a client-generated, opaque value with a 1 KB character limit that is recorded in the analytics logs.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/about-storage-analytics-logging Learn more>
pbmClientRequestId :: Lens' PutBlobMetadata (Maybe Text)
pbmClientRequestId = lens _pbmClientRequestId (\ s a -> s{_pbmClientRequestId = a})

--------------------------------------------------------------------------------
-- * Destructuring the Response

-- | Create a 'PutBlobMetadataResponse' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbmrETag' - The ETag contains a value that the client can use to perform conditional PUT operations by using the If-Match request header.
--
-- * 'pbmrLastModified' - The date/time that the blob was last modified.
--
-- * 'pbmrRequestId' - Uniquely identifies the request that was made and can be used for troubleshooting the request.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/troubleshooting-api-operations Learn more>
--
-- * 'pbmrServerEncrypted' - Whether the contents of the request are successfully encrypted using the specified algorithm
--
createPutBlobMetadataResponse
  :: Types.ETag     -- ^ 'pbmrETag'
  -> Time.UTCTime   -- ^ 'pbmrLastModified'
  -> Text           -- ^ 'pbmrRequestId'
  -> PutBlobMetadataResponse
createPutBlobMetadataResponse eTag modified reqId
  = PutBlobMetadataResponse eTag modified reqId False

-- | /See:/ 'createPutBlobMetadataResponse' smart constructor.
data PutBlobMetadataResponse = PutBlobMetadataResponse
  { _pbmrETag :: Types.ETag
  , _pbmrLastModified :: Time.UTCTime
  , _pbmrRequestId :: Text
  , _pbmrServerEncrypted :: Bool
  } deriving Show

parseResponse
  :: HTTPConduit.Response body
  -> Either Types.Error PutBlobMetadataResponse
parseResponse r = go $ HTTP.responseStatus r
  where
    go s
      | s == Status.ok200 = ok $ HTTPConduit.responseHeaders r
      | otherwise = Left $ Types.ErrorOther "Unrecognized status code"
    ok hs = Either.either (Left . Types.MarshallError) Right
          $  PutBlobMetadataResponse
          <$> Request.lookupHeader hs "ETag"
          <*> Request.lookupHeader hs "Last-Modified"
          <*> Request.lookupHeader hs "x-ms-request-id"
          <*> Request.lookupHeader hs "x-ms-request-server-encrypted"

--------------------------------------------------------------------------------
-- * Response Lenses

-- | The ETag contains a value that the client can use to perform conditional PUT operations by using the If-Match request header.
pbmrETag :: Lens' PutBlobMetadataResponse Types.ETag
pbmrETag = lens _pbmrETag (\ s a -> s{_pbmrETag = a})

-- | The date/time that the blob was last modified.
pbmrLastModified :: Lens' PutBlobMetadataResponse Time.UTCTime
pbmrLastModified = lens _pbmrLastModified (\ s a -> s{_pbmrLastModified = a})

-- | Uniquely identifies the request that was made and can be used for troubleshooting the request.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/troubleshooting-api-operations Learn more>
pbmrRequestId :: Lens' PutBlobMetadataResponse Text
pbmrRequestId = lens _pbmrRequestId (\ s a -> s{_pbmrRequestId = a})

-- | Whether the contents of the request are successfully encrypted using the specified algorithm
pbmrServerEncrypted :: Lens' PutBlobMetadataResponse Bool
pbmrServerEncrypted = lens _pbmrServerEncrypted (\ s a -> s{_pbmrServerEncrypted = a})
