{-# LANGUAGE DeriveGeneric #-}
module Data.Model.User where

import           MyPrelude

import           Crypto.Hash
import           Data.Aeson
import           GHC.Generics

newtype Password = Password Text

data Hidden = Hidden deriving (Generic)

newtype Hash = Hash (Digest SHA256)

-- User is parameterized by the hash type. User types password as Password, is stores as Hash and is transferred as Hidden
data User p =
  User { username :: Text
       , secret   :: p }
            deriving (Generic)

instance ToJSON p => ToJSON (User p)
instance FromJSON p => FromJSON (User p)

