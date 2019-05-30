{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module API
  ( API(..)
  , Version(..)
  ) where

import           Data.Aeson
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Servant.API
import           Servant.API.Generic

newtype Version = Version { version :: Text } deriving (Show, Generic, Eq)

instance ToJSON Version

instance FromJSON Version

data API route =
  API { getVersion :: route :- "version" :>  Get '[JSON] Version }
               deriving (Generic)
