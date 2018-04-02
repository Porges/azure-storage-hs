{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The <https://docs.microsoft.com/en-us/rest/api/storageservices/get-blob Get Blob> operation operation reads or downloads a blob from the system, including its metadata and properties.
module Azure.Storage.Blob.GetBlob (
  -- * Creating a Request
    createGetBlob
  , GetBlob
  , createRangeFrom
  , createRangeWithin
  , Range

  -- * Request Lenses
  , gbrBody
  , gbContainerName
  , gbBlobName
  , gbSnapshot
  , gbTimeout
  , gbRange
  , gbLeaseId
  , gbOrigin
  , gbClientRequestId

  -- * Destructuring the Response
  , createGetBlobResponse
  , GetBlobResponse

  -- * Response Lenses
  , gbrLastModified
  , gbrMetaData
  , gbrContentLength
  , gbrContentType
  , gbrETag
  , gbrContentMD5
  , gbrCacheControl
  , gbrContentDisposition
  , gbrBlobSequenceNumber
  , gbrBlobType
  , gbrRequestId
  , gbrAccessControlAllowOrigin
  , gbrAccessControlExposeHeaders
  , gbrAccessControlAllowCredentials
  , gbrServerEncrypted
) where

import qualified Azure.Storage.Request as Request
import qualified Azure.Storage.Blob.Types as Blob
import qualified Azure.Storage.Types as Types
import           Data.Monoid ((<>))
import           Lens.Micro ((^.), Lens', lens)
import           Data.Text (Text)
import           Numeric.Natural (Natural)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Status as Status
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Conduit as HTTPConduit
import qualified Data.Time as Time
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe

--------------------------------------------------------------------------------
-- * Creating a Request

-- | Select bytes starting from
createRangeFrom :: Natural -> Range
createRangeFrom = From

-- | Select bytes within a bound range
createRangeWithin :: (Natural, Natural) -> Range
createRangeWithin = uncurry Within

-- | /See:/ 'createRangeFrom' and 'createRangeWithin' smart constructors.
data Range
  = From Natural
  | Within Natural Natural

instance Types.ToBinary Range where
  toBinary (From a)     = "bytes=" <> Types.toBinary a <> "-"
  toBinary (Within a b) = "bytes=" <> Types.toBinary a <> "-" <> Types.toBinary b

-- | Create a 'GetBlob' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbContainerName' - The container where the blob resides.
--
-- * 'gbBlobName' - The name of the desired blob.
--
-- * 'gbSnapshot' - Specify the blob snapshot to retrieve
--
-- * 'gbTimeout' - The timeout parameter is expressed in seconds.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/setting-timeouts-for-blob-service-operations Learn more>
--
-- * 'gbRange' - Return only the bytes of the blob in the specified range.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/specifying-the-range-header-for-blob-service-operations Learn more>
--
-- * 'gbLeaseId' - Required if the blob has an active lease.
--
-- * 'gbOrigin' - Specifies the origin from which the request is issued.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/cross-origin-resource-sharing--cors--support-for-the-azure-storage-services Learn More>
--
-- * 'gbClientRequestId' - Provides a client-generated, opaque value with a 1 KB character limit that is recorded in the analytics logs.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/about-storage-analytics-logging Learn more>
--
createGetBlob
  :: Blob.ContainerName -- ^ 'gbContainerName'
  -> Blob.BlobName      -- ^ 'gbBlobName'
  -> GetBlob
createGetBlob cn bn = GetBlob cn bn Nothing Nothing Nothing Nothing Nothing Nothing

-- | /See:/ 'createGetBlob' smart constructor.
data GetBlob = GetBlob
  { _gbContainerName :: Blob.ContainerName
  , _gbBlobName :: Blob.BlobName
  , _gbSnapshot :: Maybe Time.UTCTime
  , _gbTimeout :: Maybe Natural
  , _gbRange :: Maybe Range
  , _gbLeaseId :: Maybe Text
  , _gbOrigin :: Maybe Text
  , _gbClientRequestId :: Maybe Text
  }

instance Request.ToRequest GetBlob where
  toMethod = const Method.methodGet
  toHeaders o
    = Request.mkBinaryPairs
    [ ("x-ms-lease-id", o ^. gbLeaseId)
    , (Header.hOrigin, o ^. gbOrigin)
    , ("x-ms-client-request-id", o ^. gbClientRequestId)
    ]
   <> Request.mkBinaryPairs
    [ ("range", o ^. gbRange) ]
  toQuery o = Map.fromList
            $ Request.mkBinaryPairs [ ("timeout", o ^. gbTimeout) ]
           <> Request.mkBinaryPairs [ ("snapshot", o ^. gbSnapshot) ]
  toPath o
    = (TE.encodeUtf8 . Blob.unContainerName $ o ^. gbContainerName)
   <> "/"
   <> (TE.encodeUtf8 . Blob.unBlobName $ o ^. gbBlobName)
  type Rs GetBlob = GetBlobResponse
  parseResponse = const parseResponse

-- | The container where the blob resides.
gbContainerName :: Lens' GetBlob Blob.ContainerName
gbContainerName = lens _gbContainerName (\ s a -> s{_gbContainerName = a})

-- | The name of the desired blob.
gbBlobName :: Lens' GetBlob Blob.BlobName
gbBlobName = lens _gbBlobName (\ s a -> s{_gbBlobName = a})

-- | Specify the blob snapshot to retrieve
gbSnapshot :: Lens' GetBlob (Maybe Time.UTCTime)
gbSnapshot = lens _gbSnapshot (\ s a -> s{_gbSnapshot = a})

-- | The timeout parameter is expressed in seconds.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/setting-timeouts-for-blob-service-operations Learn more>
gbTimeout :: Lens' GetBlob (Maybe Natural)
gbTimeout = lens _gbTimeout (\ s a -> s{_gbTimeout = a})

-- | Return only the bytes of the blob in the specified range.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/specifying-the-range-header-for-blob-service-operations Learn more>
gbRange :: Lens' GetBlob (Maybe Range)
gbRange = lens _gbRange (\ s a -> s{_gbRange = a})

-- | Required if the blob has an active lease.
gbLeaseId :: Lens' GetBlob (Maybe Text)
gbLeaseId = lens _gbLeaseId (\ s a -> s{_gbLeaseId = a})

-- | Specifies the origin from which the request is issued.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/cross-origin-resource-sharing--cors--support-for-the-azure-storage-services Learn More>
gbOrigin :: Lens' GetBlob (Maybe Text)
gbOrigin = lens _gbOrigin (\ s a -> s{_gbOrigin = a})

-- | Provides a client-generated, opaque value with a 1 KB character limit that is recorded in the analytics logs.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/about-storage-analytics-logging Learn more>
gbClientRequestId :: Lens' GetBlob (Maybe Text)
gbClientRequestId = lens _gbClientRequestId (\ s a -> s{_gbClientRequestId = a})

--------------------------------------------------------------------------------
-- * Destructuring the Response

-- | Create a 'GetBlobResponse' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrBody' - Blob content
--
-- * 'gbrLastModified' - The date/time that the blob was last modified.
--
-- * 'gbrMetaData' - Name-value pairs associated with the blob as metadata.
--
-- * 'gbrContentLength' - The number of bytes present in the response body.
--
-- * 'gbrContentType' - The content type specified for the blob.
--
-- * 'gbrETag' - The ETag contains a value that you can use to perform operations conditionally.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/specifying-conditional-headers-for-blob-service-operations Learn more>
--
-- * 'gbrContentMD5' - MD5 hash the client can use to check for message content integrity.
--
-- * 'gbrCacheControl' - Value previously specified for the blob.
--
-- * 'gbrContentDisposition' - Conveys additional information about how to process the response payload.
--
-- * 'gbrBlobSequenceNumber' - The current sequence number for a page blob.
--
-- * 'gbrBlobType' - Returns the blob's type.
--
-- * 'gbrRequestId' - uniquely identifies the request that was made and can be used for troubleshooting the request.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/troubleshooting-api-operations Learn more>
--
-- * 'gbrAccessControlAllowOrigin' - When request includes Origin, and a CORS rule matches: value of the origin request header in case of a match
--
-- * 'gbrAccessControlExposeHeaders' - When request includes Origin, and a CORS rule matches: response headers that are to be exposed to the client or issuer of the request
--
-- * 'gbrAccessControlAllowCredentials' - When request includes Origin, and a CORS rule matches: returns True unless all origins allowed.
--
-- * 'gbrServerEncrypted' - Whether the contents of the request are successfully encrypted using the specified algorithm
--
createGetBlobResponse
  :: BSL.ByteString   -- ^ 'gbrBody'
  -> Time.UTCTime     -- ^ 'gbrLastModified'
  -> Natural          -- ^ 'gbrContentLength'
  -> Text             -- ^ 'gbrContentType'
  -> Types.ETag       -- ^ 'gbrETag'
  -> Blob.BlobType    -- ^ 'gbrBlobType'
  -> Text             -- ^ 'gbrRequestId'
  -> GetBlobResponse
createGetBlobResponse body modified cLen cType eTag type_ reqId
  = GetBlobResponse body modified mempty cLen cType eTag Nothing Nothing Nothing
    Nothing type_ reqId Nothing Nothing Nothing False

-- | /See:/ 'createGetBlobResponse' smart constructor.
data GetBlobResponse = GetBlobResponse
  { _gbrBody :: BSL.ByteString
  , _gbrLastModified :: Time.UTCTime
  , _gbrMetaData :: Blob.Metadata
  , _gbrContentLength :: Natural
  , _gbrContentType :: Text
  , _gbrETag :: Types.ETag
  , _gbrContentMD5 :: Maybe Text
  , _gbrCacheControl :: Maybe Text
  , _gbrContentDisposition :: Maybe Text
  , _gbrBlobSequenceNumber :: Maybe Natural
  , _gbrBlobType :: Blob.BlobType
  , _gbrRequestId :: Text
  , _gbrAccessControlAllowOrigin :: Maybe Text
  , _gbrAccessControlExposeHeaders :: Maybe Text
  , _gbrAccessControlAllowCredentials :: Maybe Bool
  , _gbrServerEncrypted :: Bool
  } deriving Show

parseResponse :: HTTPConduit.Response BSL.ByteString -> Either Types.Error GetBlobResponse
parseResponse r = go $ HTTP.responseStatus r
  where
    body = HTTP.responseBody r
    go s
      | s == Status.ok200 = ok $ HTTPConduit.responseHeaders r
      | s == Status.partialContent206 = ok $ HTTPConduit.responseHeaders r
      | s == Status.preconditionFailed412 = Left Types.BlobUnderLease
      | otherwise = Left $ Types.ErrorOther "Unrecognized status code"
    ok hs = Either.either (Left . Types.MarshallError) Right
          $  GetBlobResponse body
          <$> Request.lookupHeader hs "Last-Modified"
          <*> (pure . getMetaData $ hs )
          <*> Request.lookupHeader hs "Content-Length"
          <*> Request.lookupHeader hs "Content-Type"
          <*> Request.lookupHeader hs "ETag"
          <*> Request.lookupHeaderOptional hs "Content-MD5"
          <*> Request.lookupHeaderOptional hs "Cache-Control"
          <*> Request.lookupHeaderOptional hs "Content-Disposition"
          <*> Request.lookupHeaderOptional hs "x-ms-blob-sequence-number"
          <*> Request.lookupHeader hs "x-ms-blob-type"
          <*> Request.lookupHeader hs "x-ms-request-id"
          <*> Request.lookupHeaderOptional hs "Access-Control-Allow-Origin"
          <*> Request.lookupHeaderOptional hs "Access-Control-Expose-Headers"
          <*> Request.lookupHeaderOptional hs "Access-Control-Allow-Credentials"
          <*> Request.lookupHeader hs "x-ms-server-encrypted"
    getMetaData hs
      = Map.map TE.decodeUtf8
      . Map.mapKeys TE.decodeUtf8
      . Map.fromList
      . map (\(k,v) -> (Maybe.fromJust k, v) )
      . filter (\h -> Maybe.isJust . fst $ h)
      . map (\(k,v) -> (BS.stripPrefix "x-ms-meta-" k, v))
      . map (\(k,v) -> (CI.original k, v) )
      $ hs

--------------------------------------------------------------------------------
-- * Response Lenses

-- | Blob content
gbrBody :: Lens' GetBlobResponse BSL.ByteString
gbrBody = lens _gbrBody (\ s a -> s{_gbrBody = a})

-- | The date/time that the blob was last modified.
gbrLastModified :: Lens' GetBlobResponse Time.UTCTime
gbrLastModified = lens _gbrLastModified (\ s a -> s{_gbrLastModified = a})

-- | Name-value pairs associated with the blob as metadata.
gbrMetaData :: Lens' GetBlobResponse Blob.Metadata
gbrMetaData = lens _gbrMetaData (\ s a -> s{_gbrMetaData = a})

-- | The number of bytes present in the response body.
gbrContentLength :: Lens' GetBlobResponse Natural
gbrContentLength = lens _gbrContentLength (\ s a -> s{_gbrContentLength = a})

-- | The content type specified for the blob.
gbrContentType :: Lens' GetBlobResponse Text
gbrContentType = lens _gbrContentType (\ s a -> s{_gbrContentType = a})

-- | The ETag contains a value that you can use to perform operations conditionally.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/specifying-conditional-headers-for-blob-service-operations Learn more>
gbrETag :: Lens' GetBlobResponse Types.ETag
gbrETag = lens _gbrETag (\ s a -> s{_gbrETag = a})

-- | MD5 hash the client can use to check for message content integrity.
gbrContentMD5 :: Lens' GetBlobResponse (Maybe Text)
gbrContentMD5 = lens _gbrContentMD5 (\ s a -> s{_gbrContentMD5 = a})

-- | Value previously specified for the blob.
gbrCacheControl :: Lens' GetBlobResponse (Maybe Text)
gbrCacheControl = lens _gbrCacheControl (\ s a -> s{_gbrCacheControl = a})

-- | Conveys additional information about how to process the response payload.
gbrContentDisposition :: Lens' GetBlobResponse (Maybe Text)
gbrContentDisposition = lens _gbrContentDisposition (\ s a -> s{_gbrContentDisposition = a})

-- | The current sequence number for a page blob.
gbrBlobSequenceNumber :: Lens' GetBlobResponse (Maybe Natural)
gbrBlobSequenceNumber = lens _gbrBlobSequenceNumber (\ s a -> s{_gbrBlobSequenceNumber = a})

-- | Returns the blob's type.
gbrBlobType :: Lens' GetBlobResponse Blob.BlobType
gbrBlobType = lens _gbrBlobType (\ s a -> s{_gbrBlobType = a})

-- | uniquely identifies the request that was made and can be used for troubleshooting the request.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/troubleshooting-api-operations Learn more>
gbrRequestId :: Lens' GetBlobResponse Text
gbrRequestId = lens _gbrRequestId (\ s a -> s{_gbrRequestId = a})

-- | When request includes Origin, and a CORS rule matches: value of the origin request header in case of a match
gbrAccessControlAllowOrigin :: Lens' GetBlobResponse (Maybe Text)
gbrAccessControlAllowOrigin = lens _gbrAccessControlAllowOrigin (\ s a -> s{_gbrAccessControlAllowOrigin = a})

-- | When request includes Origin, and a CORS rule matches: response headers that are to be exposed to the client or issuer of the request
gbrAccessControlExposeHeaders :: Lens' GetBlobResponse (Maybe Text)
gbrAccessControlExposeHeaders = lens _gbrAccessControlExposeHeaders (\ s a -> s{_gbrAccessControlExposeHeaders = a})

-- | When request includes Origin, and a CORS rule matches: returns True unless all origins allowed.
gbrAccessControlAllowCredentials :: Lens' GetBlobResponse (Maybe Bool)
gbrAccessControlAllowCredentials = lens _gbrAccessControlAllowCredentials (\ s a -> s{_gbrAccessControlAllowCredentials = a})

-- | Whether the contents of the request are successfully encrypted using the specified algorithm
gbrServerEncrypted :: Lens' GetBlobResponse Bool
gbrServerEncrypted = lens _gbrServerEncrypted (\ s a -> s{_gbrServerEncrypted = a})
