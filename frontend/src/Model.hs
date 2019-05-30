{-# LANGUAGE DeriveGeneric #-}
module Model where

import           GHC.Generics (Generic)
import           Miso

import           API          (Version)

data Model = Model { version :: Maybe Version
                   , uri     :: URI }
           deriving (Eq, Generic)
