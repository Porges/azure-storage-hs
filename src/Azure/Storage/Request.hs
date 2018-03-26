{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Azure.Storage.Request where

import qualified Network.HTTP.Client as HTTP
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Method as Method
import qualified Azure.Storage.Types as Types
import qualified Data.Either as Either
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Data.Monoid ((<>))
import qualified Data.CaseInsensitive as CI

type ToRequest a = (ToMethod a, ToPath a, ToQuery a, ToHeaders a, ToBody a)

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
class ToMethod a where
  toMethod :: a -> Method

type Path = BS.ByteString
class ToPath a where
  toPath :: a -> Path
  toPath = const mempty

type QueryParams = Map BS.ByteString BS.ByteString
class ToQuery a where
  toQuery :: a -> QueryParams
  toQuery = const mempty

type RequestHeaders = Header.RequestHeaders
class ToHeaders a where
  toHeaders :: a -> RequestHeaders
  toHeaders = const mempty

class ToBody a where
  toBody :: a -> HTTP.RequestBody
  toBody = const . HTTP.RequestBodyBS $ mempty

mkBinaryPairs :: Types.ToBinary v => [(k, Maybe v)] -> [(k, BS.ByteString)]
mkBinaryPairs xs = foldMap (uncurry go) xs
  where
    go h = Maybe.maybe mempty (\v -> pure (h, Types.toBinary v))

type Response = HTTP.Response BSL.ByteString
class FromResponse a where
  parseResponse :: Response -> Either Types.Error a

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
