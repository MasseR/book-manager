{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.App where

import           MyPrelude

import           Data.Environment    (Environment)

import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Crypto.Random.Types (MonadRandom)
import UnliftIO (MonadUnliftIO)

newtype AppM a =
  AppM (ReaderT Environment IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Environment
             , MonadRandom
             , MonadIO
             , MonadCatch
             , MonadThrow
             , MonadUnliftIO
             )

runAppM :: Environment -> AppM a -> IO a
runAppM env (AppM f) = runReaderT f env

