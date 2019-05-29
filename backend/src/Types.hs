{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
  ( App(..)
  , AppM
  , runApp
  ) where

import Control.Monad.Reader
import GHC.Generics

data App = App
         deriving (Generic)

newtype AppM a =
  AppM { runApp :: ReaderT App IO a }
    deriving (Functor, Applicative, Monad, MonadReader App)

