{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE TypeOperators #-}
module API
  ( API(..)
  , Version(..)
  ) where

import           Data.Aeson
import           Data.Text           (Text)
import qualified Data.Version        as Version
import           GHC.Generics        (Generic)
import           Servant.API
import           Servant.API.Generic

import qualified API.Users           as Users

import           Data.GenValidity
import           Test.QuickCheck     (Arbitrary (..), Positive (..), suchThat)

newtype Version = Version { version :: Version.Version }
  deriving (Show, Generic, Eq)
  deriving (Arbitrary) via (ViaGenValid Version)

instance ToJSON Version
instance FromJSON Version

newtype ViaGenValid a = ViaGenValid a

instance GenValid a => Arbitrary (ViaGenValid a) where
  arbitrary = ViaGenValid <$> genValid
  shrink (ViaGenValid v) = ViaGenValid <$> shrinkValid v

instance Validity Version where
  validate (Version v) = allPositive <> nonEmpty
    where
      allPositive = check (all (>=0) $ Version.versionBranch v) "Version branch all positive"
      nonEmpty = check (Version.versionBranch v /= []) "Non-empty"

instance GenValid Version where
  genValid = Version . Version.makeVersion . map getPositive <$> (arbitrary `suchThat` (not . null))
  shrinkValid (Version v) = to <$> filter (not . null) (shrink from)
    where
      from = map Positive (Version.versionBranch v)
      to = Version . Version.makeVersion . map getPositive

data API route =
  API { getVersion :: route :- "version" :>  Get '[JSON] Version
      , users      :: route :- "users" :> ToServant Users.API AsApi  }
               deriving (Generic)
