{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Handler.Login
  ( Route
  , render
  , Action(..)
  , updateModel
  )
  where

import           Miso
import           Servant.API

import           MyPrelude
import           View

type Route = "login" :> View Action

data Action = SetUsername Text
            | SetPassword Text

updateModel :: m -> Action -> Effect Action m
updateModel m _ = noEff m

render :: Page m Action
render = baseView{content}
  where
    content _model = [div_ [] [form_ [] [ username, password ]]]
    username = input_ [placeholder_ "Username"]
    password = input_ [placeholder_ "Password"]
