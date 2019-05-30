{-# LANGUAGE DataKinds #-}
module API.Auth where

import           Servant.Auth    as SA

import           Data.Model.User

type Authorized = Auth '[SA.BasicAuth, SA.JWT, SA.Cookie] (User Hidden)

