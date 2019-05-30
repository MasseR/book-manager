{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Login
  ( Route
  , render
  )
  where

import           Miso
import           Servant.API
import           View

type Route a = "login" :> View a

render :: Page m a
render = baseView{content}
  where
    content _model = div_ [] [text "Login"]
