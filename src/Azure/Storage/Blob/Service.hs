{-# LANGUAGE OverloadedStrings #-}

module Azure.Storage.Blob.Service where

import qualified Azure.Storage.Blob.Types as Blob
import qualified Azure.Storage.Types as Types
import qualified Azure.Storage.Request as Request
import qualified Azure.Storage.Authentication as Auth
import qualified Network.HTTP.Client as HTTP
import           Control.Monad.Except (liftIO, MonadIO)
import qualified Data.Either as Either

issueRequest
  :: (Request.ToRequest blobReq, Request.FromResponse a, MonadIO m)
  => Blob.Client -> blobReq -> m (Either (Types.Error, Request.Response) a)
issueRequest client blobReq = do
  let creds = Blob.blobCreds client
  let mgr = Blob.blobHttp client
  let reqUnsigned = Request.createRequest blobReq $ Blob.blobReq client

  request <- Auth.signRequest creds reqUnsigned
  responseRaw <- liftIO (HTTP.httpLbs request mgr)
  let responseParsed = Request.parseResponse responseRaw
  let response = Either.either (\e -> Left $ (e,responseRaw)) Right responseParsed

  return response
