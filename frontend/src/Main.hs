{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           API                              (Version)
import           Control.Lens                     ((^?), _Just)
import           Data.Aeson
import           Data.Generics.Product            (field)
import           Data.Proxy                       (Proxy (..))
import           GHC.Generics                     (Generic)
import           JSDOM.Types                      (MonadDOM, toJSString)
import           Language.Javascript.JSaddle.Warp
import           Miso
import           MyPrelude
import qualified Network.Client.HTTP              as HTTP
import           Servant.API
import           Servant.Links

type Route = Home :<|> Login
type Home = View Action
type Login = "login" :> View Action

goHome :: Action
goHome = goto @Home @Route Proxy Proxy

goLogin :: Action
goLogin = goto @Login @Route Proxy Proxy

goto :: (IsElem endpoint api, HasLink endpoint, MkLink endpoint Link ~ Link) => Proxy api -> Proxy endpoint -> Action
goto a b = ChangeURI (linkURI (safeLink a b))

data Model = Model { version :: Maybe Version
                   , uri     :: URI }
           deriving (Eq, Generic)

data Action = Init
            | HandleURI URI
            | ChangeURI URI
            | UpdateVersion (HTTP.Response (Maybe Version))
            | UpdateLoginStatus (HTTP.Response ByteString)
            | NoOp

updateModel :: Model -> Action -> Effect Action Model
updateModel m = \case
  NoOp -> noEff m
  Init -> batchEff m [ getVersion, getLoginStatus ]
  HandleURI uri -> noEff (m{uri})
  ChangeURI uri -> m <# do
    pushURI uri
    return (HandleURI uri)
  UpdateVersion HTTP.Response{content=Just v} -> noEff m{version=v}
  UpdateVersion HTTP.Response{} -> noEff m{version=Nothing}
  UpdateLoginStatus HTTP.Response{status=200} -> m <# pure goHome
  UpdateLoginStatus HTTP.Response{status=401} -> m <# pure goLogin
  UpdateLoginStatus HTTP.Response{status=_} -> noEff m

getVersion :: MonadDOM m => m Action
getVersion = do
  response <- HTTP.get "http://localhost:8088/version"
  pure (UpdateVersion (decodeStrict <$> response))

getLoginStatus :: MonadDOM m => m Action
getLoginStatus = do
  response <- HTTP.get "http://localhost:8088/users/login"
  pure (UpdateLoginStatus response)

viewModel :: Model -> View Action
viewModel m = div_ [] [ footer ]
  where
    footer = div_ [] [ viewVersion ]
    viewVersion :: View Action
    viewVersion = span_ [] [ (text . maybe "No connection" toJSString) (m ^? field @"version" . _Just . field @"version") ]

main :: IO ()
main = do
  let port = 8081
  putStrLn $ "Running on port " <> tshow port
  run port $ do
    model <- mkModel
    startApp App{..}
  where
    mkModel = Model <$> pure Nothing <*> getCurrentURI
    update = flip updateModel
    view = viewModel
    subs = [ uriSub HandleURI ]
    events = defaultEvents
    mountPoint = Nothing
    initialAction = Init
