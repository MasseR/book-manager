{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- orphan instances because I don't want sqlite dependency on api
module DB.Users
  ( User(..)
  , insertUser
  , getUser
  , hashPassword
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

instance FromField Hash where
  fromField f = Hash <$> fromField f

instance FromField Username where
  fromField f = Username <$> fromField f

hashPassword :: MonadRandom m => User Password -> m (User Hash)
hashPassword User{secret=Password p,..} = do
  secret <- Hash <$> BCrypt.hashPassword 6 (encodeUtf8 p)
  pure User{..}

insertUser :: (MonadRandom m, WithDB env m) => User Hash -> m ()
insertUser user@User{username} =
  unlessM (isJust <$> getUser username) (insert user)
  where
    insert = execute "insert into users (username, hash) values (?, ?)"

getUser :: WithDB env m => Username -> m (Maybe (User Hash))
getUser username = query "select hash from users where username = ?" (Only username) >>= \case
  [Only h] -> pure (Just (User username h))
  _ -> pure Nothing

validateHash :: Hash -> Password -> Bool
validateHash (Hash h) (Password p) = BCrypt.validatePassword (encodeUtf8 p) h

validateUser :: User Hash -> Password -> Bool
validateUser User{secret} = validateHash secret
