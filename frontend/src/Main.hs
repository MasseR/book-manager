{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import           Data.Aeson
import           Data.Proxy                       (Proxy (..))
import           JSDOM.Types                      (MonadDOM)
import           Language.Javascript.JSaddle.Warp
import           Miso
import           Miso.String
import           Servant.API                      hiding (header)
import           Servant.Links
import qualified Control.Lens as L
import Data.Generics.Product

import           API                              (Version)
import           Model
import           MyPrelude
import qualified Network.Client.HTTP              as HTTP
import           View                             (Page (..), renderView)

import qualified Handler.Home                     as Home
import qualified Handler.Login                    as Login

type Route = Home.Route Action :<|> Login.Route

goHome :: Action
goHome = goto @(Home.Route Action) @Route Proxy Proxy

goLogin :: Action
goLogin = goto @Login.Route @Route Proxy Proxy

goto :: (IsElem endpoint api, HasLink endpoint, MkLink endpoint Link ~ Link) => Proxy api -> Proxy endpoint -> Action
goto a b = ChangeURI (linkURI (safeLink a b))


data Action = Init
            | HandleURI URI
            | ChangeURI URI
            | UpdateVersion (HTTP.Response (Maybe Version))
            | UpdateLoginStatus (HTTP.Response ByteString)
            | HomeAction Home.Action
            | LoginAction Login.Action
            | NoOp
            deriving (Generic)

lensed :: (action -> Action) -> L.Lens' Model model -> Model -> (model -> action -> Effect action model) -> action -> Effect Action Model
lensed a l m f act =
  bimap a (\x -> L.set l x m) (f (L.view l m) act)

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
  LoginAction act -> lensed LoginAction (typed @Login.Model) m Login.updateModel act
  HomeAction act -> lensed HomeAction (typed @Home.Model) m Home.updateModel act

getVersion :: MonadDOM m => m Action
getVersion = do
  response <- HTTP.get "http://localhost:8088/version"
  pure (UpdateVersion (decodeStrict <$> response))

getLoginStatus :: MonadDOM m => m Action
getLoginStatus = do
  response <- HTTP.get "http://localhost:8088/users/login"
  pure (UpdateLoginStatus response)

baseView :: Page Model Action
baseView = Page{..}
  where
    header Model{title} = [h1_ [] [text (toMisoString title)]]
    content _model = []
    footer Model{version} = [div_ [] [text (toMisoString (show version))]]

viewModel :: Model -> View Action
viewModel model = either (const the404) id (runRoute @Route Proxy handler uri model)
  where
    the404 = div_ [] [text "Route not found"]
    handler = renderView (dimap homeModel HomeAction Home.render)
         -- This might not be feasible in the long run.
         -- What happens if a handler needs access to the top level action?
         :<|> renderView (baseView <> dimap loginModel LoginAction Login.render)

main :: IO ()
main = do
  let port = 8081
  putStrLn $ "Running on port " <> tshow port
  run port $ do
    model <- mkModel
    startApp App{..}
  where
    mkModel = Model
                <$> pure Nothing
                <*> getCurrentURI
                <*> pure "Home"
                <*> Home.initialModel
                <*> Login.initialModel
    update = flip updateModel
    view = viewModel
    subs = [ uriSub HandleURI ]
    events = defaultEvents
    mountPoint = Nothing
    initialAction = Init
