{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Model.User where

import           MyPrelude

import           Data.Aeson
import           GHC.Generics

newtype Password = Password Text

data Hidden = Hidden deriving (Generic)

newtype Hash = Hash ByteString

newtype Username = Username Text deriving (ToJSON, FromJSON, Show, Eq)

-- User is parameterized by the hash type. User types password as Password, is stores as Hash and is transferred as Hidden
data User p =
  User { username :: Username
       , secret   :: p }
            deriving (Generic)

instance ToJSON p => ToJSON (User p)
instance FromJSON p => FromJSON (User p)

