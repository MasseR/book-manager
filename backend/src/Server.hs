{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Server
  ( application
  ) where

import           Control.Exception      (try)
import           Control.Monad.Except   (ExceptT (..))
import           Control.Monad.Reader
import qualified Data.Text              as T
import           Data.Version           (showVersion)
import           MyPrelude
import qualified Paths_backend
import           Servant
import           Servant.API.Generic
import           Servant.Server.Generic

import           API
import           Types

handler :: API (AsServerT AppM)
handler = API {..}
  where
    getVersion = pure (Version (T.pack . showVersion $ Paths_backend.version))

api :: Proxy (ToServantApi API)
api = genericApi @API Proxy

application :: App -> Application
application st = serve api (hoistServer api nat (genericServerT handler))
  where
    nat f = Handler . ExceptT . try $ runReaderT (runApp f) st
