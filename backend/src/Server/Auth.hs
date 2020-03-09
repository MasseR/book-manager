{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- orphan instances because servant-auth-server has the classes I need
module Server.Auth where

import           Data.Bool           (bool)
import           Data.Text.Encoding  (decodeUtf8)
import           Servant.Auth.Server


import           Data.Model.User
import           Database.Users
import           MyPrelude
import Data.Environment (Environment)
-- import           Types

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult (User Hidden))

instance FromBasicAuthData (User Hidden) where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

instance FromJWT (User Hidden)
instance ToJWT (User Hidden)

authenticate :: Environment -> BasicAuthData -> IO (AuthResult (User Hidden))
authenticate app (BasicAuthData u p) = flip runReaderT app $
  maybe NoSuchUser verifyUser <$> getUser (Username (decodeUtf8 u))
  where
    validate user = validateUser user (Password (decodeUtf8 p))
    authenticated :: User a -> AuthResult (User Hidden)
    authenticated User{..} = Authenticated User{secret=Hidden,..}
    verifyUser :: User Hash -> AuthResult (User Hidden)
    verifyUser user@User{..} = bool BadPassword (authenticated user) (validate user)
