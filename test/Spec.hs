{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens                                        (over, set)
import qualified Data.Aeson                                          as Aeson
import           Data.Fixed                                          (mod')
import qualified Data.HashMap.Strict                                 as H
import qualified Data.HashSet                                        as HS
import qualified Data.Scientific                                     as Scientific
import qualified Data.Text                                           as Text
import           Hedgehog
import qualified Hedgehog.Gen                                        as Gen
import qualified Hedgehog.Gen.JSON.Constrained.Internal.InternalSpec as InternalSpec
import           Hedgehog.Gen.JSON.JSONSchema
import qualified Hedgehog.Gen.JSON.JSONSpec                          as JSONSpec
import qualified Hedgehog.Range                                      as Range
import qualified Prelude                                             as P
import           Protolude
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Text.Regex.Posix

tests :: TestTree
tests = testGroup "Hedgehog.Gen.JSON tests" [InternalSpec.tests, JSONSpec.tests]

main :: IO ()
main = defaultMain tests
