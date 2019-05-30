{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

-- import           JSDOM.Custom.XMLHttpRequest
import           API                              (Version)
import           Control.Lens                     (to, (^.), (^?), _Just)
import           Data.Aeson
import           Data.Generics.Product            (field)
import           GHC.Generics                     (Generic)
import           JSDOM.Types
import           Language.Javascript.JSaddle.Warp
import           Miso
import           MyPrelude
import           Network.Client.HTTP

newtype Model = Model { version :: Maybe Version } deriving (Eq, Generic)

data Action = Init
            | HandleURI URI
            | UpdateVersion (Maybe Version)
            | NoOp

updateModel :: Model -> Action -> Effect Action Model
updateModel m = \case
  Init -> m <# do
    response <- get "http://localhost:8088/version"
    pure (UpdateVersion (response ^. responseBody . to decodeStrict))
  UpdateVersion v -> noEff m{version=v}
  _ -> noEff m

viewModel :: Model -> View Action
viewModel m = div_ [] [ footer ]
  where
    footer = div_ [] [ viewVersion ]
    viewVersion :: View Action
    viewVersion = span_ [] [ maybe (text "No connection") (text . toJSString) (m ^? field @"version" . _Just . field @"version") ]

main :: IO ()
main = do
  let port = 8081
  putStrLn $ "Running on port " <> tshow port
  run port $ do
    model <- mkModel
    startApp App{..}
  where
    mkModel = Model <$> pure Nothing
    update = flip updateModel
    view = viewModel
    subs = [ uriSub HandleURI ]
    events = defaultEvents
    mountPoint = Nothing
    initialAction = Init
