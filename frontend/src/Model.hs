{-# LANGUAGE DeriveGeneric #-}
module Model where

import           GHC.Generics  (Generic)
import           Miso

import           API           (Version)
import           MyPrelude

import qualified Handler.Login as Login
import qualified Handler.Home as Home

data Model = Model { version    :: Maybe Version
                   , uri        :: URI
                   , title      :: Text
                   , homeModel  :: Home.Model
                   , loginModel :: Login.Model}
           deriving (Eq, Generic)
