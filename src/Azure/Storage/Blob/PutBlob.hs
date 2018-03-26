{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The Put Blob operation creates a new block, page, or append blob
-- , or updates the content of an existing block blob.
module Azure.Storage.Blob.PutBlob (
  -- * Options
    createPutBlobOptions
  , PutBlobOptions
  , pboContainerName
  , pboBlobName
  , pboTimeout
  , pboContentEncoding
  , pboContentType
  , pboContentMD5
  , pboCacheControl
  , pboMetadata
  , pboContentDisposition
  , pboOrigin
  , pboLeaseId
  , pboClientRequestId

  -- * Block blob request
  , createPutBlockBlob
  , PutBlockBlob
  , pbbBody
  , pbbOptions

  -- * Append blob request
  , createPutAppendBlob
  , PutAppendBlob
  , pabOptions

  -- * Page blob request
  , createPutPageBlob
  , PutPageBlob
  , ppbBlobContentLength
  , ppbBlobSequenceNumber
  , ppbOptions

  -- * blob response
  , createPutBlobResponse
  , PutBlobResponse
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

--------------------------------------------------------------------------------
-- * Options

-- | Creates a value of 'PutBlobOptions' with the minimal fields set.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pboContainerName' - Container where the blob should be put
--
-- * 'pboBlobName' - Name of the blob
--
-- * 'pboTimeout' - The timeout parameter is expressed in seconds.
--
-- * 'pboContentEncoding' - Specifies which content encodings have been applied to the blob.
-- This value is returned to the client when the Get Blob operation is performed on the blob resource.
-- The client can use this value when returned to decode the blob content.
--
-- * 'pboContentType' - The MIME content type of the blob. The default type is application/octet-stream.
--
-- * 'pboContentMD5' - An MD5 hash of the blob content. This hash is used to verify the integrity of the blob during transport.
-- When this header is specified, service verifies arrived payload yields specified hash.
-- The service auto generates the value when not set (visible in GetBlob).
--
-- * 'pboCacheControl' - The Blob service stores this value but does not use or modify it.
--
-- * 'pboMetadata' - Required if the blob has an active lease.
--
-- * 'pboContentDisposition' - Specifies how to process the response payload, and also can be used to attach additional metadata.
-- For example, if set to attachment, it indicates that the user-agent should not display the response,
-- but instead show a Save As dialog with a filename other than the blob name specified.
--
-- * 'pboOrigin' - Specifies the origin from which the request is issued.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/cross-origin-resource-sharing--cors--support-for-the-azure-storage-services Learn More>
--
-- * 'pboClientRequestId' - Provides a client-generated, opaque value with a 1 KB character limit that is recorded in the analytics logs.
-- <https://docs.microsoft.com/en-us/rest/api/storageservices/about-storage-analytics-logging Learn more>
--
createPutBlobOptions
  :: Blob.ContainerName -- ^ 'pboContainerName'
  -> Blob.BlobName      -- ^ 'pboBlobName'
  -> PutBlobOptions
createPutBlobOptions cn bn
  = PutBlobOptions cn bn Nothing Nothing Nothing Nothing Nothing mempty Nothing
    Nothing Nothing Nothing

-- | Common options for any type of blob.
-- /See:/ 'createPutBlobOptions' smart constructor.
data PutBlobOptions = PutBlobOptions
  { _pboContainerName :: Blob.ContainerName
  , _pboBlobName :: Blob.BlobName
  , _pboTimeout :: Maybe Natural
  , _pboContentEncoding :: Maybe Text
  , _pboContentType :: Maybe Text
  , _pboContentMD5 :: Maybe Text
  , _pboCacheControl :: Maybe Text
  , _pboMetadata :: Blob.Metadata
  , _pboLeaseId :: Maybe Text
  , _pboContentDisposition :: Maybe Text
  , _pboOrigin :: Maybe Text
  , _pboClientRequestId :: Maybe Text
  }

LensTH.makeLenses ''PutBlobOptions

--------------------------------------------------------------------------------
-- * Utils

putBlobMethod :: Request.Method
putBlobMethod = Method.methodPut

putBlobPath :: PutBlobOptions -> Request.Path
putBlobPath o
  = (TE.encodeUtf8 . Blob.unContainerName $ o ^. pboContainerName)
 <> "/"
 <> (TE.encodeUtf8 . Blob.unBlobName $ o ^. pboBlobName)

putBlobQuery :: PutBlobOptions -> Request.QueryParams
putBlobQuery o = Map.fromList $ Request.mkBinaryPairs [ ("timeout", o ^. pboTimeout) ]

optionHeaders :: PutBlobOptions -> Request.RequestHeaders
optionHeaders o = staticHeaders <> metaHeaders
  where
    staticHeaders
      = Request.mkBinaryPairs
      [ (Header.hContentType, o ^. pboContentType)
      , (Header.hContentEncoding, o ^. pboContentEncoding)
      , (Header.hContentMD5, o ^. pboContentMD5)
      , (Header.hCacheControl, o ^. pboCacheControl)
      , ("x-ms-blob-content-disposition", o ^. pboContentDisposition)
      , (Header.hOrigin, o ^. pboOrigin)
      , ("x-ms-client-request-id", o ^. pboClientRequestId)
      ]
    metaHeaders
      = Map.toList
      . Map.mapKeys (CI.mk . BS.append "x-ms-meta-" . TE.encodeUtf8)
      . Map.map TE.encodeUtf8
      $ o ^. pboMetadata

--------------------------------------------------------------------------------
-- * Block blob

-- | Create a 'PutBlockBlob' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbbBody' -  Content of the blob
--
-- * 'pbbOptions' - options
--
createPutBlockBlob
  :: BSL.ByteString -- ^ 'pbbBody'
  -> PutBlobOptions -- ^ 'pbbOptions'
  -> PutBlockBlob
createPutBlockBlob = PutBlockBlob

-- | /See:/ 'createPutBlockBlob' smart constructor.
data PutBlockBlob = PutBlockBlob
  { _pbbBody :: BSL.ByteString
  , _pbbOptions :: PutBlobOptions
  }
LensTH.makeLenses ''PutBlockBlob

instance Request.ToMethod PutBlockBlob where
  toMethod = const putBlobMethod
instance Request.ToBody PutBlockBlob where
  toBody = HTTPConduit.RequestBodyLBS . _pbbBody
instance Request.ToHeaders PutBlockBlob where
  toHeaders x
    = ("x-ms-blob-type", Types.toBinary Blob.BlockBlob)
    : (optionHeaders . _pbbOptions $ x)
instance Request.ToQuery PutBlockBlob where
  toQuery = putBlobQuery . _pbbOptions
instance Request.ToPath PutBlockBlob where
  toPath = putBlobPath . _pbbOptions

--------------------------------------------------------------------------------
-- * Append blob

-- | Create a 'PutAppendBlob' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pabOptions' - options
--
createPutAppendBlob
  :: PutBlobOptions -- ^ 'pabOptions'
  -> PutAppendBlob
createPutAppendBlob = PutAppendBlob

-- | /See:/ 'createPutAppendBlob' smart constructor.
data PutAppendBlob = PutAppendBlob
  { _pabOptions :: PutBlobOptions
  }
LensTH.makeLenses ''PutAppendBlob

instance Request.ToMethod PutAppendBlob where
  toMethod = const putBlobMethod
instance Request.ToBody PutAppendBlob
instance Request.ToHeaders PutAppendBlob where
  toHeaders x
    = ("x-ms-blob-type", Types.toBinary Blob.AppendBlob)
    : (optionHeaders . _pabOptions $ x)
instance Request.ToQuery PutAppendBlob where
  toQuery = putBlobQuery . _pabOptions
instance Request.ToPath PutAppendBlob where
  toPath = putBlobPath . _pabOptions

--------------------------------------------------------------------------------
-- * Page blob

-- | Create a 'PutPageBlob' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ppbBlobContentLength' - Specifies the maximum size for the page blob, up to 8 TB.
--
-- * 'ppbBlobSequenceNumber' - Service defaults to 0.
--
-- * 'ppbOptions' - options
--
createPutPageBlob
  :: Natural        -- ^ 'ppbBlobContentLength'
  -> Maybe Natural  -- ^ 'ppbBlobSequenceNumber'
  -> PutBlobOptions -- ^ 'ppbOptions'
  -> PutPageBlob
createPutPageBlob = PutPageBlob

-- | /See:/ 'createPutPageBlob' smart constructor.
data PutPageBlob = PutPageBlob
  { _ppbBlobContentLength :: Natural
  , _ppbBlobSequenceNumber :: Maybe Natural
  , _ppbOptions :: PutBlobOptions
  }
LensTH.makeLenses ''PutPageBlob

instance Request.ToMethod PutPageBlob where
  toMethod = const putBlobMethod
instance Request.ToBody PutPageBlob
instance Request.ToHeaders PutPageBlob where
  toHeaders b
    = ("x-ms-blob-type", Types.toBinary Blob.PageBlob)
    : (optionHeaders . _ppbOptions $ b)
   <> (Request.mkBinaryPairs
        [ ("x-ms-blob-content-length", Just $ b ^. ppbBlobContentLength)
        , ("x-ms-blob-sequence-number", b ^.  ppbBlobSequenceNumber)
        ]
      )
instance Request.ToQuery PutPageBlob where
  toQuery = putBlobQuery . _ppbOptions
instance Request.ToPath PutPageBlob where
  toPath = putBlobPath . _ppbOptions

--------------------------------------------------------------------------------
-- * blob response

-- | Create a 'PutBlobResponse' with minimal fields.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbrETag' - The ETag contains a value that the client can use to perform conditional PUT operations by using the If-Match request header.
--
-- * 'pbrLastModified' - The date/time that the blob was last modified.
--
-- * 'pbrContentMD5' - This header is returned for a block blob so the client can check the integrity of message content.
--
-- * 'pbrRequestId' - This header uniquely identifies the request that was made and can be used for troubleshooting the request.
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
LensTH.makeLenses ''PutBlobResponse

instance Request.FromResponse PutBlobResponse where
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
