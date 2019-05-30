{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Server.Users where

import           Servant
import           Servant.Server.Generic

import           API.Users              (API (..))
import           DB.Users
import           MyPrelude
import           Server.Auth
import           Types

handler :: API (AsServerT AppM)
handler = API {..}
  where
    postUser user = NoContent <$ (insertUser =<< hashPassword user)
    getLogin user = requireUser user (\_ -> pure NoContent)
