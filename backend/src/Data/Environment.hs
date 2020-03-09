{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Data.Environment where

import           Crypto.JOSE.JWK (JWK)
import           Database.Internal     (Connection, HasConnection (..))

import           GHC.Generics    (Generic)
import Data.Generics.Product

data Environment =
  Environment { envConnection :: Connection
              , envKey        :: JWK
              }
  deriving (Generic)

instance HasConnection Environment where
  connection = typed @Connection
