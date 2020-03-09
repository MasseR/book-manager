{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Test.Server where

import           Test.Hspec

import           Control.Lens             (set)
import           Data.Generics.Product
import           Network.Wai.Handler.Warp (testWithApplication)
import qualified Paths_backend

import           DB.Internal              (withConnection)
import           Servant.Auth.Server      (generateKey)
import           Data.Environment

import           API                      (getVersion)
import           Client
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Servant.Client           (parseBaseUrl)

import           MyPrelude

-- System under test
import           Server

withServer :: (ApiEnv -> IO a) -> IO a
withServer f = withConnection ":memory:" $ \connection -> do
  jwtKey <- generateKey
  let app = Environment connection jwtKey
  testWithApplication (pure (application app)) $ \port -> do
    manager <- newManager defaultManagerSettings
    base <- set (field @"baseUrlPort") port <$> parseBaseUrl "http://localhost"
    let env = ApiEnv manager base
    f env


spec :: Spec
spec = around withServer $ describe "Server" $
  it "Returns a version" $ \env -> do
    got <- runReaderT (runClient (getVersion api)) env
    got `shouldBe` Right currentVersion
