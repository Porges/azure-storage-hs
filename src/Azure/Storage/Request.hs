{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Azure.Storage.Request where

import qualified Network.HTTP.Client as HTTP
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Method as Method
import qualified Azure.Storage.Types as Types
import qualified Data.Either as Either
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Data.Monoid ((<>))
import qualified Data.CaseInsensitive as CI
import           Data.Proxy (Proxy)
import qualified Data.Conduit as C
import           Control.Monad.Trans.Resource (ResourceT)

createRequest :: ToRequest a => a -> HTTP.Request -> HTTP.Request
createRequest a r
  = HTTP.setQueryString qs r
  { HTTP.method         = toMethod a
  , HTTP.path           = toPath a
  , HTTP.requestHeaders = toHeaders a <> HTTP.requestHeaders r
  , HTTP.requestBody    = toBody a
  }
  where
    qs = Map.toList . Map.map Just $ toQuery a

type Method = Method.Method
type Path = BS.ByteString
type QueryParams = Map BS.ByteString BS.ByteString
type RequestHeaders = Header.RequestHeaders

newtype RsBody = RsBody { _streamBody :: C.ConduitM () BS.ByteString (ResourceT IO) () }
instance Show RsBody where
  show = const "RsBody { _streamBody :: C.ConduitM () BS.ByteString (ResourceT IO) () }"

class ToRequest a where

  toMethod :: a -> Method

  toPath :: a -> Path
  toPath = const mempty

  toQuery :: a -> QueryParams
  toQuery = const mempty

  toHeaders :: a -> RequestHeaders
  toHeaders = const mempty

  toBody :: a -> HTTP.RequestBody
  toBody = const . HTTP.RequestBodyBS $ mempty

  -- | the final typed response for the service
  type Rs a :: *
  parseResponse
    :: Proxy a -- ^ for injectivity reasons
    -> HTTP.Response RsBody
    -> Either Types.Error (Rs a)

mkBinaryPairs :: Types.ToBinary v => [(k, Maybe v)] -> [(k, BS.ByteString)]
mkBinaryPairs xs = foldMap (uncurry go) xs
  where
    go h = Maybe.maybe mempty (\v -> pure (h, Types.toBinary v))

type ResponseHeaders = Header.ResponseHeaders

lookupHeader
  :: Types.FromBinary a
  => ResponseHeaders -> Header.HeaderName -> Either Text a
lookupHeader h k
  = Maybe.maybe (Left errNoKey) parseKey
  $ lookup k h
  where
    k' = TE.decodeUtf8 . CI.original $ k
    errNoKey = "Expected key (" <> k' <> ") not found!"
    errBadKey = "Expected key (" <> k' <> ") to have type: "
    parseKey v
      = Either.either (Left . mappend errBadKey) Right
      $ Types.parseBinary v

lookupHeaderOptional
  :: Types.FromBinary a
  => ResponseHeaders -> Header.HeaderName -> Either Text (Maybe a)
lookupHeaderOptional h k
  = Maybe.maybe (pure Nothing) (const . fmap Just $ lookupHeader h k)
  $ lookup k h
