{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Server.Auth where

import           Servant.Auth.Server
import Data.Text.Encoding (decodeUtf8)
import Data.Bool (bool)


import MyPrelude
import Types
import Data.Model.User
import DB.Users


authenticate :: App -> BasicAuthData -> IO (AuthResult (User Hidden))
authenticate app (BasicAuthData u p) = flip runReaderT app $
  maybe NoSuchUser verifyUser <$> getUser (Username (decodeUtf8 u))
  where
    validate user = validateUser user (Password (decodeUtf8 p))
    authenticated :: User a -> AuthResult (User Hidden)
    authenticated User{..} = Authenticated User{secret=Hidden,..}
    verifyUser :: User Hash -> AuthResult (User Hidden)
    verifyUser user@User{..} = bool BadPassword (authenticated user) (validate user)
