{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Azure.Storage.Authentication as Auth
import Azure.Storage.Blob as AB 

import           Control.Monad.Trans.Resource (runResourceT, MonadResource)
import           Control.Monad.Except (runExceptT, liftIO)
import           Data.Conduit ((.|), runConduitRes)
import qualified Data.Conduit.Combinators as CC
import           Data.Either.Validation (Validation(..))
import           Data.Semigroup ((<>))
import           Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Client as HTTP
import           Test.Tasty (defaultMain, testGroup, TestTree, withResource)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.HUnit (testCase, testCaseSteps, assertBool, assertEqual, (@?=))
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = defaultMain tests
tests =
    withResource start stop $ \mgrAct -> testGroup "Integration tests" [blobTests mgrAct]

    where

    start = do
        -- return an HTTP manager to share
        HTTP.newManager HTTP.defaultManagerSettings

    stop _mgr =
        -- no cleanup needed
        pure ()

blobTests mgrAct =
    testGroup "Blob" [container bcAct]
    where
    bcAct = do
        mgr <- mgrAct
        let (Just bc) = AB.getBlobClient Auth.developmentStorageAccount mgr
        return bc

container :: IO AB.BlobClient -> TestTree
container bcAct =
    let runBC f = bcAct >>= f in

    testGroup "Container"
    [ testCase "Deleting nonexistent container returns false" $ runBC $ \bc -> do
        deleted <- runExceptT (AB.deleteContainer bc (AB.containerName_ "does-not-exist"))
        deleted @?= Right False

    , testCaseSteps "Create/list/delete container" $ \step -> runBC $ \bc -> do
        let containerName = AB.containerName_ "created"

        step "Create container"
        created <- runExceptT (AB.createContainer bc containerName)
        created @?= Right True

        step "Create container again"
        created <- runExceptT (AB.createContainer bc containerName)
        created @?= Right False

        step "Find container in list"
        found <- runConduitRes (AB.listContainers bc .| CC.elem containerName)
        found @?= True

        step "Delete container"
        deleted <- runExceptT (AB.deleteContainer bc containerName)
        deleted @?= Right True
    ]
