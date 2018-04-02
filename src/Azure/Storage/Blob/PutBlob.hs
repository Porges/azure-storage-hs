{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The <https://docs.microsoft.com/en-us/rest/api/storageservices/put-blob Put Blob> operation creates a new block, page, or append blob
-- , or updates the content of an existing block blob.
module Azure.Storage.Blob.PutBlob (
  -- * Creating a Request
    createPutBlob
  , PutBlob
  , createBlockBlob
  , createAppendBlob
  , createPageBlob
  , BlobType

  -- * Request Lenses
  , pbContainerName
  , pbBlobName
  , pbBlobType
  , pbTimeout
  , pbContentEncoding
  , pbContentType
  , pbContentMD5
  , pbCacheControl
  , pbMetadata
  , pbContentDisposition
  , pbOrigin
  , pbLeaseId
  , pbClientRequestId

  -- * Destructuring the Response
  , createPutBlobResponse
  , PutBlobResponse

  -- * Response Lenses
  , pbrETag
  , pbrLastModified
  , pbrContentMD5
  , pbrRequestId
  , pbrAccessControlAllowOrigin
  , pbrAccessControlExposeHeaders
  , pbrAccessControlAllowCredentials
  , pbrServerEncrypted
) where

import qualified Azure.Storage.Request as Request
import qualified Azure.Storage.Blob.Types as Blob
import qualified Azure.Storage.Types as Types
import           Data.Monoid ((<>))
import           Lens.Micro ((^.), Lens', lens)
import           Data.Text (Text)
import           Numeric.Natural (Natural)
import qualified Data.ByteString as BS
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

--------------------------------------------------------------------------------
-- * Creating a Request

-- | create a block blob
createBlockBlob
  :: HTTP.RequestBody -- ^ body of the blob
  -> BlobType
createBlockBlob = BlockBlob

-- | Create an append blob
createAppendBlob :: BlobType
createAppendBlob = AppendBlob

-- | Create a page blob
createPageBlob
  :: Natural          -- ^ Blob content length
  -> (Maybe Natural)  -- ^ Blob sequence number
  -> BlobType
createPageBlob = PageBlob

-- | /See:/ smart constructors: 'createBlockBlob', 'createAppendBlob', 'createPageBlob'.
-- Sum-type for various types of blobs, and their parameters.
data BlobType
  = BlockBlob HTTP.RequestBody
  | AppendBlob
  | PageBlob Natural (Maybe Natural)
instance Show BlobType where
  show (BlockBlob _) = "BlockBlob { HTTP.RequestBody }"
  show AppendBlob = "AppendBlob"
  show (PageBlob cl sn)
    = "PageBlob { contentLength = "
   <> show cl
   <> " seqeuence = "
   <> show sn
   <> " }"

-- | Creates a value of 'PutBlobOptions' with the minimal fields set.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbContainerName' - Container where the blob should be put
--
-- * 'pbBlobName' - Name of the blob
--
-- * 'pbBlobType' - Type of blob. See 'BlobType'.
--
-- * 'pbTimeout' - The timeout parameter is expressed in seconds.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/setting-timeouts-for-blob-service-operations Learn more>
--
-- * 'pbContentEncoding' - Specifies which content encodings have been applied to the blob.
-- This value is returned to the client when the Get Blob operation is performed on the blob resource.
-- The client can use this value when returned to decode the blob content.
--
-- * 'pbContentType' - The MIME content type of the blob. The default type is application/octet-stream.
--
-- * 'pbContentMD5' - An MD5 hash of the blob content. This hash is used to verify the integrity of the blob during transport.
-- When this header is specified, service verifies arrived payload yields specified hash.
-- The service auto generates the value when not set (visible in GetBlob).
--
-- * 'pbCacheControl' - The Blob service stores this value but does not use or modify it.
--
-- * 'pbMetadata' - Name-value pairs associated with the blob as metadata.
--
-- * 'pbLeaseId' - Required if the blob has an active lease.
--
-- * 'pbContentDisposition' - Specifies how to process the response payload, and also can be used to attach additional metadata.
-- For example, if set to attachment, it indicates that the user-agent should not display the response,
-- but instead show a Save As dialog with a filename other than the blob name specified.
--
-- * 'pbOrigin' - Specifies the origin from which the request is issued.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/cross-origin-resource-sharing--cors--support-for-the-azure-storage-services Learn More>
--
-- * 'pbClientRequestId' - Provides a client-generated, opaque value with a 1 KB character limit that is recorded in the analytics logs.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/about-storage-analytics-logging Learn more>
--
createPutBlob
  :: Blob.ContainerName -- ^ 'pbContainerName'
  -> Blob.BlobName      -- ^ 'pbBlobName'
  -> BlobType           -- ^ 'pbBlobType'
  -> PutBlob
createPutBlob cn bn
  = PutBlob cn bn Nothing Nothing Nothing Nothing Nothing mempty Nothing
    Nothing Nothing Nothing

-- | /See:/ 'createPutBlob' smart constructor.
data PutBlob = PutBlob
  { _pbContainerName :: Blob.ContainerName
  , _pbBlobName :: Blob.BlobName
  , _pbTimeout :: Maybe Natural
  , _pbContentEncoding :: Maybe Text
  , _pbContentType :: Maybe Text
  , _pbContentMD5 :: Maybe Text
  , _pbCacheControl :: Maybe Text
  , _pbMetadata :: Blob.Metadata
  , _pbLeaseId :: Maybe Text
  , _pbContentDisposition :: Maybe Text
  , _pbOrigin :: Maybe Text
  , _pbClientRequestId :: Maybe Text
  , _pbBlobType :: BlobType
  } deriving Show

instance Request.ToRequest PutBlob where
  toMethod = const Method.methodPut
  toBody x = case x ^. pbBlobType of
              (BlockBlob body) -> body
              _ -> HTTPConduit.RequestBodyLBS mempty
  toHeaders o = staticHeaders <> metaHeaders <> blobTypeHeaders (o ^. pbBlobType)
    where
      staticHeaders
        = Request.mkBinaryPairs
        [ (Header.hContentType, o ^. pbContentType)
        , (Header.hContentEncoding, o ^. pbContentEncoding)
        , (Header.hContentMD5, o ^. pbContentMD5)
        , (Header.hCacheControl, o ^. pbCacheControl)
        , ("x-ms-lease-id", o ^. pbLeaseId)
        , ("x-ms-blob-content-disposition", o ^. pbContentDisposition)
        , (Header.hOrigin, o ^. pbOrigin)
        , ("x-ms-client-request-id", o ^. pbClientRequestId)
        ]
      metaHeaders
        = Map.toList
        . Map.mapKeys (CI.mk . BS.append "x-ms-meta-" . TE.encodeUtf8)
        . Map.map TE.encodeUtf8
        $ o ^. pbMetadata
      blobTypeHeaders (BlockBlob _)
        = ("x-ms-blob-type", Types.toBinary Blob.BlockBlob)
        : []
      blobTypeHeaders AppendBlob
        = pure ("x-ms-blob-type", Types.toBinary Blob.AppendBlob)
      blobTypeHeaders (PageBlob cl sn)
        = ("x-ms-blob-type", Types.toBinary Blob.PageBlob)
        : (Request.mkBinaryPairs
            [ ("x-ms-blob-content-length", Just $ cl)
            , ("x-ms-blob-sequence-number", sn)
            ]
          )

  toQuery o = Map.fromList $ Request.mkBinaryPairs [ ("timeout", o ^. pbTimeout) ]
  toPath o
    = (TE.encodeUtf8 . Blob.unContainerName $ o ^. pbContainerName)
    <> "/"
    <> (TE.encodeUtf8 . Blob.unBlobName $ o ^. pbBlobName)
  type Rs PutBlob = PutBlobResponse
  parseResponse = const parseResponse

--------------------------------------------------------------------------------
-- * Request Lenses

-- | Container where the blob should be put
pbContainerName :: Lens' PutBlob Blob.ContainerName
pbContainerName = lens _pbContainerName (\ s a -> s{_pbContainerName = a})

-- | Name of the blob
pbBlobName :: Lens' PutBlob Blob.BlobName
pbBlobName = lens _pbBlobName (\ s a -> s{_pbBlobName = a})

-- | Type of blob. See 'BlobType'.
pbBlobType :: Lens' PutBlob BlobType
pbBlobType = lens _pbBlobType (\ s a -> s{_pbBlobType = a})

-- | The timeout parameter is expressed in seconds.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/setting-timeouts-for-blob-service-operations Learn more>
pbTimeout :: Lens' PutBlob (Maybe Natural)
pbTimeout = lens _pbTimeout (\ s a -> s{_pbTimeout = a})

-- | 'pbContentEncoding' - Specifies which content encodings have been applied to the blob.
-- This value is returned to the client when the Get Blob operation is performed on the blob resource.
-- The client can use this value when returned to decode the blob content.
pbContentEncoding :: Lens' PutBlob (Maybe Text)
pbContentEncoding = lens _pbContentEncoding (\ s a -> s{_pbContentEncoding = a})

-- | The MIME content type of the blob. The default type is application/octet-stream.
pbContentType :: Lens' PutBlob (Maybe Text)
pbContentType = lens _pbContentType (\ s a -> s{_pbContentType = a})

-- | 'pbContentMD5' - An MD5 hash of the blob content. This hash is used to verify the integrity of the blob during transport.
-- When this header is specified, service verifies arrived payload yields specified hash.
-- The service auto generates the value when not set (visible in GetBlob).
pbContentMD5 :: Lens' PutBlob (Maybe Text)
pbContentMD5 = lens _pbContentMD5 (\ s a -> s{_pbContentMD5 = a})

-- | The Blob service stores this value but does not use or modify it.
pbCacheControl :: Lens' PutBlob (Maybe Text)
pbCacheControl = lens _pbCacheControl (\ s a -> s{_pbCacheControl = a})

-- | Name-value pairs associated with the blob as metadata.
pbMetadata :: Lens' PutBlob Blob.Metadata
pbMetadata = lens _pbMetadata (\ s a -> s{_pbMetadata = a})

-- | Required if the blob has an active lease.
pbLeaseId :: Lens' PutBlob (Maybe Text)
pbLeaseId = lens _pbLeaseId (\ s a -> s{_pbLeaseId = a})

-- | Specifies how to process the response payload, and also can be used to attach additional metadata.
-- For example, if set to attachment, it indicates that the user-agent should not display the response,
-- but instead show a Save As dialog with a filename other than the blob name specified.
pbContentDisposition :: Lens' PutBlob (Maybe Text)
pbContentDisposition = lens _pbContentDisposition (\ s a -> s{_pbContentDisposition = a})

-- | Specifies the origin from which the request is issued.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/cross-origin-resource-sharing--cors--support-for-the-azure-storage-services Learn More>
pbOrigin :: Lens' PutBlob (Maybe Text)
pbOrigin = lens _pbOrigin (\ s a -> s{_pbOrigin = a})

-- | Provides a client-generated, opaque value with a 1 KB character limit that is recorded in the analytics logs.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/about-storage-analytics-logging Learn more>
pbClientRequestId :: Lens' PutBlob (Maybe Text)
pbClientRequestId = lens _pbClientRequestId (\ s a -> s{_pbClientRequestId = a})

--------------------------------------------------------------------------------
-- * Destructuring the Response

-- | Create a 'PutBlobResponse' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbrETag' - The ETag contains a value that the client can use to perform conditional PUT operations by using the If-Match request header.
--
-- * 'pbrLastModified' - The date/time that the blob was last modified.
--
-- * 'pbrContentMD5' - Returned for a block blob so the client can check the integrity of message content.
--
-- * 'pbrRequestId' - Uniquely identifies the request that was made and can be used for troubleshooting the request.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/troubleshooting-api-operations Learn more>
--
-- * 'pbrAccessControlAllowOrigin' - When request includes Origin, and a CORS rule matches: value of the origin request header in case of a match
--
-- * 'pbrAccessControlExposeHeaders' - When request includes Origin, and a CORS rule matches: response headers that are to be exposed to the client or issuer of the request
--
-- * 'pbrAccessControlAllowCredentials' - When request includes Origin, and a CORS rule matches: returns True unless all origins allowed.
--
-- * 'pbrServerEncrypted' - Whether the contents of the request are successfully encrypted using the specified algorithm
--
createPutBlobResponse
  :: Types.ETag    -- ^ 'pbrETag'
  -> Time.UTCTime -- ^ 'pbrLastModified'
  -> Text         -- ^ 'pbrRequestId'
  -> PutBlobResponse
createPutBlobResponse eTag modified reqId
  = PutBlobResponse eTag modified Nothing reqId Nothing Nothing Nothing False

-- | /See:/ 'createPutBlobResponse' smart constructor.
data PutBlobResponse = PutBlobResponse
  { _pbrETag :: Types.ETag
  , _pbrLastModified :: Time.UTCTime
  , _pbrContentMD5 :: Maybe Text
  , _pbrRequestId :: Text
  , _pbrAccessControlAllowOrigin :: Maybe Text
  , _pbrAccessControlExposeHeaders :: Maybe Text
  , _pbrAccessControlAllowCredentials :: Maybe Bool
  , _pbrServerEncrypted :: Bool
  } deriving Show

parseResponse
  :: HTTPConduit.Response body
  -> Either Types.Error PutBlobResponse
parseResponse r = go $ HTTP.responseStatus r
  where
    go s
      | s == Status.created201 = ok $ HTTPConduit.responseHeaders r
      | s == Status.requestEntityTooLarge413 = Left Types.BlobTooLarge
      | s == Status.preconditionFailed412 = Left Types.BlobUnderLease
      | otherwise = Left $ Types.ErrorOther "Unrecognized status code"
    ok hs = Either.either (Left . Types.MarshallError) Right
          $  PutBlobResponse
          <$> Request.lookupHeader hs "ETag"
          <*> Request.lookupHeader hs "Last-Modified"
          <*> Request.lookupHeaderOptional hs "Content-MD5"
          <*> Request.lookupHeader hs "x-ms-request-id"
          <*> Request.lookupHeaderOptional hs "Access-Control-Allow-Origin"
          <*> Request.lookupHeaderOptional hs "Access-Control-Expose-Headers"
          <*> Request.lookupHeaderOptional hs "Access-Control-Allow-Credentials"
          <*> Request.lookupHeader hs "x-ms-request-server-encrypted"

--------------------------------------------------------------------------------
-- * Response Lenses

-- | The ETag contains a value that the client can use to perform conditional PUT operations by using the If-Match request header.
pbrETag :: Lens' PutBlobResponse Types.ETag
pbrETag = lens _pbrETag (\ s a -> s{_pbrETag = a})

-- | The date/time that the blob was last modified.
pbrLastModified :: Lens' PutBlobResponse Time.UTCTime
pbrLastModified = lens _pbrLastModified (\ s a -> s{_pbrLastModified = a})

-- | Returned for a block blob so the client can check the integrity of message content.
pbrContentMD5 :: Lens' PutBlobResponse (Maybe Text)
pbrContentMD5 = lens _pbrContentMD5 (\ s a -> s{_pbrContentMD5 = a})

-- | Uniquely identifies the request that was made and can be used for troubleshooting the request.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/troubleshooting-api-operations Learn more>
pbrRequestId :: Lens' PutBlobResponse Text
pbrRequestId = lens _pbrRequestId (\ s a -> s{_pbrRequestId = a})

-- | When request includes Origin, and a CORS rule matches: value of the origin request header in case of a match
pbrAccessControlAllowOrigin :: Lens' PutBlobResponse (Maybe Text)
pbrAccessControlAllowOrigin = lens _pbrAccessControlAllowOrigin (\ s a -> s{_pbrAccessControlAllowOrigin = a})

-- | When request includes Origin, and a CORS rule matches: response headers that are to be exposed to the client or issuer of the request
pbrAccessControlExposeHeaders :: Lens' PutBlobResponse (Maybe Text)
pbrAccessControlExposeHeaders = lens _pbrAccessControlExposeHeaders (\ s a -> s{_pbrAccessControlExposeHeaders = a})

-- | When request includes Origin, and a CORS rule matches: returns True unless all origins allowed.
pbrAccessControlAllowCredentials :: Lens' PutBlobResponse (Maybe Bool)
pbrAccessControlAllowCredentials = lens _pbrAccessControlAllowCredentials (\ s a -> s{_pbrAccessControlAllowCredentials = a})

-- | Whether the contents of the request are successfully encrypted using the specified algorithm
pbrServerEncrypted :: Lens' PutBlobResponse Bool
pbrServerEncrypted = lens _pbrServerEncrypted (\ s a -> s{_pbrServerEncrypted = a})
