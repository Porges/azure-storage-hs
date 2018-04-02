{-# LANGUAGE OverloadedStrings #-}

module Azure.Storage.Blob.Service where

import qualified Azure.Storage.Blob.Types as Blob
import qualified Azure.Storage.Types as Types
import qualified Azure.Storage.Request as Request
import qualified Azure.Storage.Authentication as Auth
import qualified Network.HTTP.Client as HTTP
import           Control.Monad.Except (liftIO, MonadIO)
import           Data.Proxy (Proxy(Proxy))

issueRequest
  :: (Request.ToRequest blobReq, MonadIO m)
  => Blob.Client -> blobReq -> m (Either Types.Error (Request.Rs blobReq))
issueRequest client blobReq = do
  request <- Auth.signRequest creds reqUnsigned
  liftIO $ print request
  responseRaw <- liftIO (HTTP.httpLbs request mgr)
  liftIO $ print responseRaw
  return $ Request.parseResponse (p blobReq) responseRaw
  where
    creds = Blob.blobCreds client
    mgr = Blob.blobHttp client
    reqUnsigned = Request.createRequest blobReq $ Blob.blobReq client
    p :: Request.ToRequest a => a -> Proxy a
    p = const Proxy