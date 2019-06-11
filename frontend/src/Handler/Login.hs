{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Handler.Login
  ( Route
  , render
  , Action(..)
  , Model(..)
  , updateModel
  , initialModel
  )
  where

import           Miso
import           Miso.String
import           Servant.API hiding (header)
import           JSDOM.Types                 (FromJSString(..))

import           MyPrelude
import           View
import Network.Client.HTTP (Response, get')

type Route = "login" :> View Action

data Action = SetUsername JSString
            | SetPassword JSString
            | Login (Response ByteString)
            | Submit
            | NoOp

data Model =
  Model { username :: JSString
        , password :: JSString
        }
           deriving (Generic, Eq, Show)

updateModel :: Model -> Action -> Effect Action Model
updateModel m = \case
  NoOp -> noEff m
  SetUsername t -> noEff (m{username=t})
  SetPassword t -> noEff (m{password=t})
  Submit -> m <# do
    putStrLn "foo"
    Login <$> get' "http://localhost:8088/users/login" (fromJSString $ username m) (fromJSString $ password m)
  Login r -> m <# do
    putStrLn $ "Login? " <> tshow r
    pure NoOp

initialModel :: Monad m => m Model
initialModel = pure (Model "" "")

render :: Page Model Action
render = Page { header = const [], content = content, footer = const [] }
  where
    content _model = [div_ [] [div_ [] [ username, password, submit ]]]
    username = input_ [onInput SetUsername, placeholder_ "Username"]
    password = input_ [onInput SetPassword, placeholder_ "Password"]
    submit = input_ [onClick Submit, type_ "Submit"]
