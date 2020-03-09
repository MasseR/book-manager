module Server.Users where

import           Servant
import           Servant.Server.Generic

import           API.Users              (API (..))
import           DB.Users
import           MyPrelude
import           Types

handler :: API (AsServerT AppM)
handler =
  API { postUser = postUser
      -- , getLogin = \u -> _
      }
  where
    postUser user = NoContent <$ (insertUser =<< hashPassword user)
    -- getLogin user = liftIO (print user) >> pure NoContent -- A dummy endpoint for logging in


