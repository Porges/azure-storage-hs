{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | The Get Blob operation fetches any type of blob in a container.
module Azure.Storage.Blob.GetBlob (
  -- * Request
    createGetBlob
  , gbrBody
  , gbContainerName
  , gbBlobName
  , gbSnapshot
  , gbTimeout
  , gbRange
  , gbLeaseId
  , gbOrigin
  , gbClientRequestId
  , GetBlob

  , createRangeFrom
  , createRangeWithin
  , Range

  -- * Response
  , createGetBlobResponse
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
  , GetBlobResponse
) where

import qualified Azure.Storage.Request as Request
import qualified Azure.Storage.Blob.Types as Blob
import qualified Azure.Storage.Types as Types
import           Data.Monoid ((<>))
import           Lens.Micro ((^.))
import qualified Lens.Micro.TH as LensTH
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
-- * Response

-- | Create a 'GetBlobResponse' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbrBody' - Blob content
--
-- * 'gbrLastModified' -The date/time that the blob was last modified.
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
-- * 'gbrCacheControl' - This header is returned if it was previously specified for the blob.
--
-- * 'gbrContentDisposition' - Conveys additional information about how to process the response payload.
--
-- * 'gbrBlobSequenceNumber' - The current sequence number for a page blob.
--
-- * 'gbrBlobType' - Returns the blob's type.
--
-- * 'gbrRequestId' - This header uniquely identifies the request that was made and can be used for troubleshooting the request.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/troubleshooting-api-operations Learn more>
--
-- * 'gbrAccessControlAllowOrigin' - When request includes Origin, and a CORS rule matches: value of the origin request header in case of a match
--
-- * 'gbrAccessControlExposeHeaders' - When request includes Origin, and a CORS rule matches: response headers that are to be exposed to the client or issuer of the request
--
-- * 'gbrAccessControlAllowCredentials' -When request includes Origin, and a CORS rule matches: returns True unless all origins allowed.
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

-- /See:/ 'createGetBlobResponse' smart constructor.
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

LensTH.makeLenses ''GetBlobResponse

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
-- * Request

-- | Select bytes starting from
createRangeFrom :: Natural -> Range
createRangeFrom = From

-- | Select bytes within a bound range
createRangeWithin :: (Natural, Natural) -> Range
createRangeWithin = uncurry Within

-- /See:/ 'createRangeFrom' and 'createRangeWithin' smart constructors.
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

-- /See:/ 'createGetBlob' smart constructor.
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

LensTH.makeLenses ''GetBlob

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
