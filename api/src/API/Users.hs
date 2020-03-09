{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}
module API.Users (API(..)) where


import           GHC.Generics        (Generic)
import           Servant.API
import           Servant.API.Generic

import           API.Auth
import           Data.Model.User

data API route =
  API { postUser :: route :- "create" :> ReqBody '[JSON] (User Password) :> Post '[JSON] NoContent
      -- , getLogin :: route :- Authorized :> "login" :> Get '[JSON] NoContent
      }
  deriving (Generic)
