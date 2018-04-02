{-# LANGUAGE OverloadedStrings #-}

module Azure.Storage.Types where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import           Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString as BS
import qualified Data.Maybe as Maybe
import           Numeric.Natural (Natural)
import           Text.Read (readMaybe)

-- | todo: derive instances to make this more useful
newtype ETag = ETag BS.ByteString
  deriving Show

timeRFC1123 :: Text
timeRFC1123 = "%a, %0d %b %Y %H:%M:%S GMT"

class FromBinary a where
  parseBinary :: BS.ByteString -> Either Text a

instance FromBinary Text where
  parseBinary = pure . TE.decodeUtf8
instance FromBinary BS.ByteString where
  parseBinary = pure
instance FromBinary Bool where
  parseBinary "true" = pure True
  parseBinary "false"= pure False
  parseBinary _      = Left "Bool (true|false)"
instance FromBinary Time.UTCTime where
  parseBinary bs
    = Maybe.maybe (Left $ "DateTime in RFC1123 ("<> timeRFC1123 <> ")") pure
    $ Time.parseTimeM False Time.defaultTimeLocale (T.unpack timeRFC1123) (BSC.unpack bs)
instance FromBinary ETag where
  parseBinary = pure . ETag
instance FromBinary Natural where
  parseBinary x = Maybe.maybe (Left "Natural number") pure . readMaybe . BSC.unpack $ x

class ToBinary a where
  toBinary :: a -> BS.ByteString

instance ToBinary Text where
  toBinary = TE.encodeUtf8
instance ToBinary BS.ByteString where
  toBinary = id
instance ToBinary Bool where
  toBinary True = "true"
  toBinary False = "false"
instance ToBinary Time.UTCTime where
  toBinary = BSC.pack . Time.formatTime Time.defaultTimeLocale (T.unpack timeRFC1123)
instance ToBinary Natural where
  toBinary = TE.encodeUtf8 . T.pack . show

data Error
  = MarshallError Text
  | BlobUnderLease
  | BlobTooLarge
  | ErrorOther Text
  deriving Show
