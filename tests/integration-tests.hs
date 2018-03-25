{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Azure.Storage.Authentication as Auth
import Azure.Storage.Blob as AB
import qualified Azure.Storage.Blob.Types as Types

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
import qualified Network.HTTP.Client.TLS as HTTPS
import           Test.Tasty (defaultMain, testGroup, TestTree, withResource)
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty.HUnit (testCase, testCaseSteps, assertBool, assertEqual, (@?=), assertFailure)
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = defaultMain tests
tests =
    withResource start stop $ \mgrAct -> testGroup "Integration tests" [blobTests mgrAct]

    where

    start = do
        -- return an HTTP manager to share
        HTTPS.newTlsManager

    stop _mgr =
        -- no cleanup needed
        pure ()

blobTests mgrAct =
    testGroup "Blob" [container bcAct]
    where
    bcAct = do
        mgr <- mgrAct
        let (Just bc) = AB.getClient Auth.developmentStorageAccount mgr
        let (Just pubBC) = AB.getClient (Auth.developmentStorageAccount { Auth.credentials = Auth.NoCredentials }) mgr
        return (bc, pubBC)

container :: IO (AB.Client, AB.Client) -> TestTree
container bcAct =
    let runBC f = bcAct >>= f in

    testGroup "Container"
    [ testCase "Deleting nonexistent container returns false" $ runBC $ \(bc, _) -> do
        deleted <- runExceptT (AB.deleteContainer bc (Types.ContainerName "does-not-exist"))
        deleted @?= Right False

    , testCaseSteps "Private container tests" $ \step -> runBC $ \(bc, pubBC) -> do
        let containerName = Types.ContainerName "created"

        step "Create container"
        created <- runExceptT (AB.createContainer bc containerName)
        created @?= Right True

        step "Create container again"
        created <- runExceptT (AB.createContainer bc containerName)
        created @?= Right False

        let blobName = Types.BlobName "blob"
        step "Create blob"
        blobCreated <- runExceptT (AB.putBlob bc containerName blobName "blob-data")
        blobCreated @?= Right ()

        let publicBlobName = Types.BlobName "blob-public"
        step "Create blob (public) should fail"
        blobCreated <- runExceptT (AB.putBlob pubBC containerName blobName "blob-public-data")
        case blobCreated of
            Left (AzureError "ResourceNotFound" _) -> pure ()
            _ -> assertFailure "putBlob should have failed"

        step "Find blob in list"
        foundBlob <- runConduitRes (AB.listBlobs bc containerName .| CC.elem blobName)
        foundBlob @?= True

        -- TODO: when I can handle errors during enumeration
        -- step "Find blob in list (public) should fail"
        -- foundBlob <- runConduitRes (AB.listBlobs pubBC containerName .| CC.elem blobName)
        -- foundBlob @?= False

        step "Find container in list"
        found <- runConduitRes (AB.listContainers bc .| CC.elem containerName)
        found @?= True

        step "Delete container"
        deleted <- runExceptT (AB.deleteContainer bc containerName)
        deleted @?= Right True

    , testCaseSteps "Blob-public container tests" $ \step -> runBC $ \(bc, pubBC) -> do

        let containerName = Types.ContainerName "blob-public-container"
        step "Create container"
        created <- runExceptT (AB.createContainer' bc containerName AB.defaultCreateContainerOptions { AB.containerAccess = AB.BlobsPublic })
        created @?= Right True

        let blobName = Types.BlobName "blob"
        step "Create blob"
        blobCreated <- runExceptT (AB.putBlob bc containerName blobName "blob-data")
        blobCreated @?= Right ()

        let publicBlobName = Types.BlobName "blob-public"
        step "Create blob (public) should fail"
        blobCreated <- runExceptT (AB.putBlob pubBC containerName blobName "blob-public-data")
        case blobCreated of
            Left (AzureError "ResourceNotFound" _) -> pure ()
            _ -> assertFailure "putBlob should have failed"

        step "Find blob in list"
        foundBlob <- runConduitRes (AB.listBlobs bc containerName .| CC.elem blobName)
        foundBlob @?= True

        -- TODO: when I can handle errors during enumeration
        -- step "Find blob in list (public) should fail"
        -- foundBlob <- runConduitRes (AB.listBlobs pubBC containerName .| CC.elem blobName)
        -- foundBlob @?= False

        step "Delete container"
        deleted <- runExceptT (AB.deleteContainer bc containerName)
        deleted @?= Right True

    , testCaseSteps "Public container tests" $ \step -> runBC $ \(bc, pubBC) -> do
        let containerName = Types.ContainerName "public-container"
        step "Create container"
        created <- runExceptT (AB.createContainer' bc containerName AB.defaultCreateContainerOptions { AB.containerAccess = AB.ContainerPublic })
        created @?= Right True

        let blobName = Types.BlobName "blob"
        step "Create blob"
        blobCreated <- runExceptT (AB.putBlob bc containerName blobName "blob-data")
        blobCreated @?= Right ()

        let publicBlobName = Types.BlobName "blob-public"
        step "Create blob (public) should fail"
        blobCreated <- runExceptT (AB.putBlob pubBC containerName blobName "blob-public-data")
        case blobCreated of
            Left (AzureError "ResourceNotFound" _) -> pure ()
            _ -> assertFailure "putBlob should have failed"

        step "Find blob in list"
        foundBlob <- runConduitRes (AB.listBlobs bc containerName .| CC.elem blobName)
        foundBlob @?= True

        step "Find blob in list (public) should succeed"
        foundBlob <- runConduitRes (AB.listBlobs pubBC containerName .| CC.elem blobName)
        foundBlob @?= True

        step "Delete container"
        deleted <- runExceptT (AB.deleteContainer bc containerName)
        deleted @?= Right True
    ]
