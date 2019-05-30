{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeApplications           #-}
module Network.Client.HTTP where

import           GHC.Generics                (Generic)
import           JSDOM.Custom.XMLHttpRequest
import           JSDOM.Types                 (MonadDOM)
import           MyPrelude

newtype Response a =
  Response { content :: Maybe a } deriving (Generic, Functor, Show)

get :: MonadDOM m => String -> m (Response Text)
get url = do
  req <- newXMLHttpRequest
  openSimple @_ @String req "GET" url
  send req
  Response <$> getResponseText req

