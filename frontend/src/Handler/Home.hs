{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Home
  ( Route
  , render
  ) where

import           Miso
import           View

type Route a = View a

render :: Page m a
render = baseView
