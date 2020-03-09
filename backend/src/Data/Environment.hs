{-# LANGUAGE DeriveGeneric #-}
module Data.Environment where

import           Crypto.JOSE.JWK (JWK)
import           DB.Internal     (Connection, HasDB (..))

import           GHC.Generics    (Generic)

data Environment =
  Environment { connection :: Connection
              , jwtKey     :: JWK
              }
  deriving (Generic)

instance HasDB Environment where
  getConnection = connection
