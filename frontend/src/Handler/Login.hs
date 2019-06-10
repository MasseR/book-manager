{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
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
import           Servant.API hiding (header)

import           MyPrelude
import           View

type Route = "login" :> View Action

data Action = SetUsername Text
            | SetPassword Text

data Model = Model
           deriving (Generic, Eq)

updateModel :: Model -> Action -> Effect Action Model
updateModel m _ = noEff m

initialModel :: Monad m => m Model
initialModel = pure Model

render :: Page Model Action
render = Page { header = const [], content = content, footer = const [] }
  where
    content _model = [div_ [] [form_ [] [ username, password ]]]
    username = input_ [placeholder_ "Username"]
    password = input_ [placeholder_ "Password"]
