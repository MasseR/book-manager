{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
  ( App(..)
  , AppM
  , runApp
  ) where

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Crypto.JOSE.JWK      (JWK)
import           Crypto.Random.Types  (MonadRandom)
import           GHC.Generics

import           DB.Internal

data App =
  App { connection :: Connection
      , jwtKey     :: JWK }
         deriving (Generic)

instance HasDB App where
  getConnection = connection

newtype AppM a =
  AppM { runApp :: ReaderT App IO a }
    deriving (Functor, Applicative, Monad, MonadReader App, MonadRandom, MonadIO, MonadCatch, MonadThrow)

