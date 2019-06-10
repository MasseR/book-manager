{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Home
  ( Route
  , Model
  , Action(..)
  , initialModel
  , updateModel
  , render
  ) where

import           Miso
import           MyPrelude
import           View

type Route a = View a

data Model = Model
           deriving (Generic, Eq)

initialModel :: Monad m => m Model
initialModel = pure Model

data Action = NoOp

render :: Page Model Action
render = Page { header = const [], content = const [text "home"], footer = const [] }

updateModel :: Model -> Action -> Effect Action Model
updateModel m _ = noEff m
