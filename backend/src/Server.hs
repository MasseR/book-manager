{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module Server
  ( application
  ) where

import           Control.Lens           ((^.))
import           Control.Monad.Except   (ExceptT (..))
import           Control.Monad.Reader
import           Crypto.JOSE.JWK        (JWK)
import           Data.Generics.Product  (typed)
import qualified Data.Text              as T
import           Data.Version           (showVersion)
import           MyPrelude
import qualified Paths_backend
import           Servant
import           Servant.API.Generic
import           Servant.Auth.Server    as SAS
import           Servant.Server.Generic

import           API
import           Data.Model.User
import           Server.Auth
import           Types

import qualified Server.Users           as Users

handler :: API (AsServerT AppM)
handler = API {..}
  where
    getVersion = pure (Version (T.pack . showVersion $ Paths_backend.version))
    users = toServant Users.handler

api :: Proxy (ToServantApi API)
api = genericApi @API Proxy

type Ctx = '[BasicAuthData -> IO (AuthResult (User Hidden)), CookieSettings, JWTSettings]

application :: App -> Application
application st = serveWithContext api ctx (hoistServerWithContext api (Proxy @Ctx) nat (genericServerT handler))
  where
    nat f = Handler . ExceptT . try $ runReaderT (runApp f) st
    cookieConfig = defaultCookieSettings{cookieIsSecure=NotSecure}
    jwtConfig = defaultJWTSettings (st ^. typed @JWK)
    ctx :: Context Ctx
    ctx = authenticate st :. cookieConfig :. jwtConfig :. EmptyContext
