{-# LANGUAGE RecordWildCards #-}
module Client where

import           Network.HTTP.Client    (Manager)
import           Servant.API.Generic    (fromServant)
import           Servant.Client
import           Servant.Client.Generic

import           MyPrelude

import           Control.Lens           (Lens', lens, set, view)

import           API

data ApiEnv
  = ApiEnv { manager :: !Manager
           , base    :: !BaseUrl
           }

class HasApiEnv a where
  {-# MINIMAL getApiEnv, setApiEnv | apiEnv #-}
  getApiEnv :: a -> ApiEnv
  getApiEnv = view apiEnv

  setApiEnv :: a -> ApiEnv -> a
  setApiEnv = flip (set apiEnv)

  apiEnv :: Lens' a ApiEnv
  apiEnv = lens getApiEnv setApiEnv

instance HasApiEnv ApiEnv where
  apiEnv = lens id (flip const)

runClient :: (HasApiEnv r, MonadReader r m, MonadIO m) => ClientM a -> m (Either ClientError a)
runClient c = do
  ApiEnv{..} <- view apiEnv
  let env = mkClientEnv manager base
  liftIO (runClientM c env)

api :: API (AsClientT ClientM)
api = genericClient
