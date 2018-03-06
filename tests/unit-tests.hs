{-# LANGUAGE OverloadedStrings #-}
module Main where

import Azure.Storage.Authentication as Auth

import Data.Either.Validation (Validation(..))
import Data.Semigroup ((<>))
import Hedgehog (forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [authentication]

lowerAlphaNum = Gen.element (['a'..'z'] ++ ['0'..'9'])

authentication = testGroup "Authentication" [authParsing]
authParsing = testGroup "Parsing"
    [ testProperty "AccountName must be less than 25 chars" $ property $ do
        input <- forAll (("AccountKey=1234;AccountName=" <>) <$> Gen.text (Range.constant 25 100) lowerAlphaNum)

        let parsed = Auth.parseConnectionString input
        parsed === Failure ["AccountName must be less than 25 characters long"]

    , testProperty "AccountName must be greater than 3 chars" $ property $ do
        input <- forAll (("AccountKey=1234;AccountName=" <>) <$> Gen.text (Range.constant 1 2) lowerAlphaNum)

        let parsed = Auth.parseConnectionString input
        parsed === Failure ["AccountName must be more than 3 characters long"]

    -- TODO
    -- , testProperty "AccountName must be only lowercase alphanum" $ property $ do
    --     input <- forAll (("AccountKey=1234;AccountName=" <>) <$> Gen.text (Range.constant 3 24) lowerAlphaNum)

    --     let parsed = Auth.parseConnectionString input
    --     parsed === Failure ["AccountName must be only lowercase alphanumeric"]

    ]
