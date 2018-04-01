{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Azure.Storage.Authentication
    ( AccountName
    , accountName
    , AccountKey
    , accountKey
    , developmentStorageAccount
    , StorageAccount(..)
    , Credentials(..)
    , parseConnectionString
    , signRequest
    , apiVersion
    )
where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Crypto.Hash.Algorithms (SHA256, HashAlgorithm)
import qualified Crypto.MAC.HMAC as HMAC
import           Data.ByteArray (ScrubbedBytes, ByteArrayAccess)
import           Data.ByteArray.Encoding (convertFromBase, convertToBase, Base(Base64))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import qualified Data.ConnectionString as CS
import           Data.Either.Validation (Validation(..))
import           Data.Foldable (foldl')
import           Data.Function ((&))
import           Data.List (intersperse, sort)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Time.Clock as Time
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Network.HTTP.Client as HTTP
import           Network.HTTP.Types (parseSimpleQuery)
import qualified Network.HTTP.Types.Header as H
import           Network.URI (URI(..), parseAbsoluteURI, parseURI, uriIsAbsolute)

-- | The supported version of the Azure Storage API.
--
-- Currently this is:
-- >>> apiVersion
-- "2017-07-29"
apiVersion :: ByteString
apiVersion = "2017-07-29"

validations :: [(String, a -> Bool)] -> a -> Validation [String] a
validations vs x =
    case mapMaybe (\(msg, p) -> if p x then Nothing else Just msg) vs of
        [] -> Success x
        msgs -> Failure msgs

-- | Represents an Azure Storage account name.
newtype AccountName = AccountName ByteString
    deriving (Show, Eq, Ord)

-- | Parses an 'AccountName' from 'Text' input, or fails
-- with validation errors.
accountName :: Text -> Validation [String] AccountName
accountName input = AccountName . encodeUtf8 <$> vs input
    where
    vs = validations
        [ ("AccountName must be more than 3 characters long",
            (>= 3) . Text.length)
        , ("AccountName must be less than 25 characters long",
            (<= 24) . Text.length)
        , ("AccountName must be lowercase alphanumeric",
            Text.all (\c -> c >= 'a' && c <= 'z' || c >= '0' && c <= '9'))
        ]

-- | Represents an Azure Storage account key.
--
-- This is stored as 'ScrubbedBytes' to help improve
-- security (see the 'ScrubbedBytes' documentation for details).
newtype AccountKey = AccountKey ScrubbedBytes
    deriving (Eq, Show)

-- | Parses an 'AccountKey' from 'Text' input, or fails
-- with validation errors (if the Base-64 encoding is incorrect).
accountKey :: Text -> Validation [String] AccountKey
accountKey key = AccountKey <$>
    case convertFromBase Base64 (encodeUtf8 key) of
        Left msg -> Failure ["AccountKey invalid: " ++ msg]
        Right decodedKey -> Success decodedKey

data Credentials
    = SharedKeyCredentials AccountName AccountKey
    | NoCredentials
    deriving (Show, Eq)

data StorageAccount = StorageAccount
    { credentials :: Credentials
    , blobEndpoint :: Maybe URI
    , queueEndpoint :: Maybe URI
    , tableEndpoint :: Maybe URI
    -- TODO:
    --, filesEndpoint :: Maybe URI
    }
    deriving (Show, Eq)

-- | Credentials valid for accessing the development storage account
-- (Azure Storage Emulator).
developmentStorageCredentials :: Credentials
developmentStorageCredentials =
    case SharedKeyCredentials <$>
            accountName "devstoreaccount1" <*>
            accountKey "Eby8vdM02xNOcqFlqUwJPLlmEtlCDXJ1OUzFT50uSRZ6IFsuFq2UVErCz4I6tq/K1SZFPTOtr/KBHBeksoGMGw=="
            of
    Success c -> c
    Failure _ -> error "TODO: parse at compile-time?"

-- | The development 'StorageAccount'.
developmentStorageAccount :: StorageAccount
developmentStorageAccount = StorageAccount
    { credentials = developmentStorageCredentials
    , blobEndpoint = parseAbsoluteURI "http://127.0.0.1:10000/devstoreaccount1"
    , queueEndpoint = parseAbsoluteURI "http://127.0.0.1:10001/devstoreaccount1"
    , tableEndpoint = parseAbsoluteURI "http://127.0.0.1:10002/devstoreaccount1"
    }

-- TODO: incomplete
parseConnectionString :: Text -> Validation [String] StorageAccount
parseConnectionString csIn =
    case CS.parse csIn of
        Left msg -> Failure [msg]
        Right cs -> extractAccount cs

    where

    extractAccount cs = StorageAccount <$> creds <*> blobUri <*> queueUri <*> tableUri

        where

        creds =
            SharedKeyCredentials
                <$> case Map.lookup "AccountName" cs of
                    Nothing -> Failure ["key 'AccountName' not found in connection string"]
                    Just an -> accountName an

                <*> case Map.lookup "AccountKey" cs of
                    Nothing -> Failure ["key 'AccountKey' not found in connection string"]
                    Just ak -> accountKey ak

        blobUri = extractUri "BlobEndpoint"
        queueUri = extractUri "QueueEndpoint"
        tableUri = extractUri "TableEndpoint"

        extractUri name =
            case Map.lookup name cs of
                Nothing -> Success Nothing
                Just uri ->
                    case parseURI (Text.unpack uri) of
                        Nothing -> Failure [Text.unpack name ++ " must be a valid URI"]
                        Just parsedUri ->
                            if uriIsAbsolute parsedUri
                            then Success (Just parsedUri)
                            else Failure [Text.unpack name ++ " must be an absolute URI"]

-- | Dates and signs the request.
signRequest :: MonadIO m => Credentials -> HTTP.Request -> m HTTP.Request
signRequest (SharedKeyCredentials name@(AccountName rawName) (AccountKey rawKey)) req =  do

    date <- liftIO Time.getCurrentTime
    let httpDate = formatTime defaultTimeLocale "%a, %0d %b %Y %H:%M:%S GMT" date
    let headers = ("Date", BS.pack httpDate) : HTTP.requestHeaders req
    let req' = req { HTTP.requestHeaders = headers }
    let signature =
            stringToSign name req'
            & hmacLazy rawKey -- this is really nice because the key isn't copied
            & HMAC.hmacGetDigest @SHA256
            & convertToBase Base64

    let authHeader = (H.hAuthorization, BS.concat ["SharedKey ", rawName, ":", signature])
    return (req' { HTTP.requestHeaders = authHeader : headers })
signRequest NoCredentials req = return req

-- TODO: push upstream into Cryptonite
hmacLazy
    ::( ByteArrayAccess key
      , HashAlgorithm a
      )
    => key -> LBS.ByteString -> HMAC.HMAC a
hmacLazy key lbs
    = foldl' HMAC.update (HMAC.initialize key) (LBS.toChunks lbs) & HMAC.finalize

stringToSign :: AccountName -> HTTP.Request -> LBS.ByteString
stringToSign (AccountName rawName) req =
    -- TODO: use Builder somehow?

    headers ++ canonicalizedHeaders ++ canonicalizedResource
    & intersperse "\n"
    & LBS.fromChunks

    where

    headers =
        [ HTTP.method req
        , hr H.hContentEncoding
        , hr H.hContentLanguage
        , hr H.hContentLength
        , hr H.hContentMD5
        , hr H.hContentType
        , hr H.hDate
        , hr H.hIfModifiedSince
        , hr H.hIfMatch
        , hr H.hIfNoneMatch
        , hr H.hIfUnmodifiedSince
        , hr H.hRange
        ]

    hr :: H.HeaderName -> ByteString
    hr name = maybe BS.empty id (lookup name (HTTP.requestHeaders req))

    canonicalizedHeaders :: [ByteString]
    canonicalizedHeaders =
        HTTP.requestHeaders req
        & map (\(h, v) -> (CI.foldedCase h, v))
        & filter (\(h, _) -> "x-ms-" `BS.isPrefixOf` h)
        & map (\(h, v) -> BS.concat [h, ":", v])
        & sort

    canonicalizedResource :: [ByteString]
    canonicalizedResource =
        BS.concat [ "/", rawName, "/", HTTP.path req ] : normalizedQuery

    normalizedQuery :: [ByteString]
    normalizedQuery =
        HTTP.queryString req
        & parseSimpleQuery
        & map (\(k, v) -> (CI.foldCase k, [v]))
        & Map.fromListWith (++)
        & fmap (intersperse "," . sort)
        & Map.toList
        & map (\(k, v) -> BS.concat (k : ":" : v))


