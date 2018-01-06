{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Hedgehog.Gen.JSON.Constrained.Internal.InternalSpec as InternalSpec
import qualified Hedgehog.Gen.JSON.JSONSpec                          as JSONSpec
import           Protolude
import           Test.Tasty

tests :: TestTree
tests = testGroup "Hedgehog.Gen.JSON tests" [InternalSpec.tests, JSONSpec.tests]

main :: IO ()
main = defaultMain tests
