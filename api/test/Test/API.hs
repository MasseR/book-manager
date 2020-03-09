{-# LANGUAGE TypeApplications #-}
module Test.API where

import           Test.Aeson.GenericSpecs
import           Test.Hspec
import           Test.Validity
import           Test.Validity.Aeson

import           API

settings :: Settings
settings = defaultSettings { useModuleNameAsSubDirectory = True }

spec :: Spec
spec = describe "API" $
  describe "Version type" $ do
    genValidSpec @Version
    shrinkValidSpec @Version
    jsonSpecOnValid @Version
    goldenSpecs @Version settings Proxy
