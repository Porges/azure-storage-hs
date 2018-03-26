{-# LANGUAGE OverloadedStrings #-}

module Azure.Storage.Blob.Service where

import qualified Azure.Storage.Blob.Types as Blob
import qualified Azure.Storage.Types as Types
import qualified Azure.Storage.Request as Request
import qualified Azure.Storage.Authentication as Auth
import qualified Network.HTTP.Client as HTTP
import           Control.Monad.Except (liftIO, MonadIO)

issueRequest
  :: (Request.ToRequest blobReq, Request.FromResponse a, MonadIO m)
  => Blob.Client -> blobReq -> m (Either Types.Error a)
issueRequest client blobReq = do
  let creds = Blob.blobCreds client
  let mgr = Blob.blobHttp client
  let reqUnsigned = Request.createRequest blobReq $ Blob.blobReq client

  request <- Auth.signRequest creds reqUnsigned
  liftIO $ print request
  responseRaw <- liftIO (HTTP.httpLbs request mgr)
  liftIO $ print responseRaw
  let responseParsed = Request.parseResponse responseRaw

  return responseParsed
