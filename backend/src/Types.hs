{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
  ( App(..)
  , AppM
  , runApp
  ) where

import           Control.Monad.Reader
import           GHC.Generics

import DB.Internal

newtype App = App { connection :: Connection }
         deriving (Generic)

instance HasDB App where
  getConnection = connection

newtype AppM a =
  AppM { runApp :: ReaderT App IO a }
    deriving (Functor, Applicative, Monad, MonadReader App)

