{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The <https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob-metadata Get Blob Metadata> operation returns all user-defined metadata for the specified blob or snapshot.
module Azure.Storage.Blob.GetBlobMetadata (
  -- * Creating a Request
    createGetBlobMetadata
  , GetBlobMetadata

  -- * Request Lenses
  , gbmContainerName
  , gbmBlobName
  , gbmSnapshot
  , gbmTimeout
  , gbmLeaseId
  , gbmClientRequestId

  -- * Destructuring the Response
  , createGetBlobMetadataResponse
  , GetBlobMetadataResponse

  -- * Response Lenses
  , gbmrLastModified
  , gbmrMetaData
  , gbmrETag
  , gbmrRequestId
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

-- | Create a 'GetBlobMetadata' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbmContainerName' - The container where the blob resides.
--
-- * 'gbmBlobName' - The name of the desired blob.
--
-- * 'gbmSnapshot' - Specify the blob snapshot to retrieve
--
-- * 'gbmTimeout' - The timeout parameter is expressed in seconds.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/setting-timeouts-for-blob-service-operations Learn more>
--
-- * 'gbmLeaseId' - Required if the blob has an active lease.
--
-- * 'gbmClientRequestId' - Provides a client-generated, opaque value with a 1 KB character limit that is recorded in the analytics logs.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/about-storage-analytics-logging Learn more>
--
createGetBlobMetadata
  :: Blob.ContainerName -- ^ 'gbmContainerName'
  -> Blob.BlobName      -- ^ 'gbmBlobName'
  -> GetBlobMetadata
createGetBlobMetadata cn bn = GetBlobMetadata cn bn Nothing Nothing Nothing Nothing

-- | /See:/ 'createGetBlobMetadata' smart constructor.
data GetBlobMetadata = GetBlobMetadata
  { _gbmContainerName :: Blob.ContainerName
  , _gbmBlobName :: Blob.BlobName
  , _gbmSnapshot :: Maybe Time.UTCTime
  , _gbmTimeout :: Maybe Natural
  , _gbmLeaseId :: Maybe Text
  , _gbmClientRequestId :: Maybe Text
  }

instance Request.ToRequest GetBlobMetadata where
  toMethod = const Method.methodGet
  toHeaders o
    = Request.mkBinaryPairs
    [ ("x-ms-lease-id", o ^. gbmLeaseId)
    , ("x-ms-client-request-id", o ^. gbmClientRequestId)
    ]
  toQuery o = Map.fromList
            $ ("comp", "metadata")
            : Request.mkBinaryPairs [ ("timeout", o ^. gbmTimeout) ]
           <> Request.mkBinaryPairs [ ("snapshot", o ^. gbmSnapshot) ]
  toPath o
    = (TE.encodeUtf8 . Blob.unContainerName $ o ^. gbmContainerName)
   <> "/"
   <> (TE.encodeUtf8 . Blob.unBlobName $ o ^. gbmBlobName)
  type Rs GetBlobMetadata = GetBlobMetadataResponse
  parseResponse = const parseResponse

-- | The container where the blob resides.
gbmContainerName :: Lens' GetBlobMetadata Blob.ContainerName
gbmContainerName = lens _gbmContainerName (\ s a -> s{_gbmContainerName = a})

-- | The name of the desired blob.
gbmBlobName :: Lens' GetBlobMetadata Blob.BlobName
gbmBlobName = lens _gbmBlobName (\ s a -> s{_gbmBlobName = a})

-- | Specify the blob snapshot to retrieve
gbmSnapshot :: Lens' GetBlobMetadata (Maybe Time.UTCTime)
gbmSnapshot = lens _gbmSnapshot (\ s a -> s{_gbmSnapshot = a})

-- | The timeout parameter is expressed in seconds.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/setting-timeouts-for-blob-service-operations Learn more>
gbmTimeout :: Lens' GetBlobMetadata (Maybe Natural)
gbmTimeout = lens _gbmTimeout (\ s a -> s{_gbmTimeout = a})

-- | Required if the blob has an active lease.
gbmLeaseId :: Lens' GetBlobMetadata (Maybe Text)
gbmLeaseId = lens _gbmLeaseId (\ s a -> s{_gbmLeaseId = a})

-- | Provides a client-generated, opaque value with a 1 KB character limit that is recorded in the analytics logs.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/about-storage-analytics-logging Learn more>
gbmClientRequestId :: Lens' GetBlobMetadata (Maybe Text)
gbmClientRequestId = lens _gbmClientRequestId (\ s a -> s{_gbmClientRequestId = a})

--------------------------------------------------------------------------------
-- * Destructuring the Response

-- | Create a 'GetBlobMetadataResponse' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrmLastModified' - The date/time that the blob was last modified.
--
-- * 'gbrmMetaData' - Name-value pairs associated with the blob as metadata.
--
-- * 'gbrmETag' - The ETag contains a value that you can use to perform operations conditionally.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/specifying-conditional-headers-for-blob-service-operations Learn more>
--
-- * 'gbrmRequestId' - uniquely identifies the request that was made and can be used for troubleshooting the request.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/troubleshooting-api-operations Learn more>
--
createGetBlobMetadataResponse
  :: Time.UTCTime     -- ^ 'gbrmLastModified'
  -> Types.ETag       -- ^ 'gbrmETag'
  -> Text             -- ^ 'gbrmRequestId'
  -> GetBlobMetadataResponse
createGetBlobMetadataResponse modified eTag reqId
  = GetBlobMetadataResponse modified mempty eTag reqId

-- | /See:/ 'GetBlobMetadataResponse' smart constructor.
data GetBlobMetadataResponse = GetBlobMetadataResponse
  { _gbmrLastModified :: Time.UTCTime
  , _gbmrMetaData :: Blob.Metadata
  , _gbmrETag :: Types.ETag
  , _gbmrRequestId :: Text
  } deriving Show

parseResponse :: HTTPConduit.Response body -> Either Types.Error GetBlobMetadataResponse
parseResponse r = go $ HTTP.responseStatus r
  where
    go s
      | s == Status.ok200 = ok $ HTTPConduit.responseHeaders r
      | otherwise = Left $ Types.ErrorOther "Unrecognized status code"
    ok hs = Either.either (Left . Types.MarshallError) Right
          $  GetBlobMetadataResponse
          <$> Request.lookupHeader hs "Last-Modified"
          <*> (pure . Blob.decodeMetadata $ hs )
          <*> Request.lookupHeader hs "ETag"
          <*> Request.lookupHeader hs "x-ms-request-id"

--------------------------------------------------------------------------------
-- * Response Lenses

-- | The date/time that the blob was last modified.
gbmrLastModified :: Lens' GetBlobMetadataResponse Time.UTCTime
gbmrLastModified = lens _gbmrLastModified (\ s a -> s{_gbmrLastModified = a})

-- | Name-value pairs associated with the blob as metadata.
gbmrMetaData :: Lens' GetBlobMetadataResponse Blob.Metadata
gbmrMetaData = lens _gbmrMetaData (\ s a -> s{_gbmrMetaData = a})

-- | The ETag contains a value that you can use to perform operations conditionally.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/specifying-conditional-headers-for-blob-service-operations Learn more>
gbmrETag :: Lens' GetBlobMetadataResponse Types.ETag
gbmrETag = lens _gbmrETag (\ s a -> s{_gbmrETag = a})

-- | uniquely identifies the request that was made and can be used for troubleshooting the request.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/troubleshooting-api-operations Learn more>
gbmrRequestId :: Lens' GetBlobMetadataResponse Text
gbmrRequestId = lens _gbmrRequestId (\ s a -> s{_gbmrRequestId = a})
