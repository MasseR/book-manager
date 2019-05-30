{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- orphan instances because I don't want sqlite dependency on api
module DB.Users
  ( User(..)
  , insertUser
  , getUser
  , validateUser )
  where

import           Data.Model.User
import           DB.Internal
import           MyPrelude

import qualified Crypto.KDF.BCrypt   as BCrypt
import           Crypto.Random.Types
import           Data.Text.Encoding  (encodeUtf8)

instance ToRow (User Hash) where
  toRow User{..} = toRow (username, secret)

instance ToField Hash where
  toField (Hash h) = toField h

instance ToField Username where
  toField (Username u) = toField u

hashPassword :: MonadRandom m => User Password -> m (User Hash)
hashPassword User{secret=Password p,..} = do
  secret <- Hash <$> BCrypt.hashPassword 6 (encodeUtf8 p)
  pure User{..}

insertUser :: (MonadRandom m, WithDB env m) => User Password -> m ()
insertUser = execute "insert into users (username, hash) values (?, ?)" <=< hashPassword

getUser :: WithDB env m => Username -> m (Maybe (User Hash))
getUser = undefined

validateHash :: Hash -> Password -> Bool
validateHash (Hash a) (Password b) = BCrypt.validatePassword a (encodeUtf8 b)

validateUser :: User Hash -> Password -> Bool
validateUser User{secret} hash = validateHash secret hash
