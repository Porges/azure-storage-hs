{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Azure.Storage
Stability: experimental

 -}

module Azure.Storage where

import Data.ByteString (ByteString)

-- | The version of the REST API to use.
--
-- Currently this is:
--
-- >>> apiVersion
-- "2017-07-29"
apiVersion :: ByteString
apiVersion = "2017-07-29"
