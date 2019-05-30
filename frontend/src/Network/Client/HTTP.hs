{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Client.HTTP where

import MyPrelude
import           JSDOM.Custom.XMLHttpRequest
import           JSDOM.Types (MonadDOM)
import GHC.Generics (Generic)

newtype Response a =
  Response { content :: Maybe a } deriving (Generic, Functor, Show)

get :: MonadDOM m => String -> m (Response Text)
get url = do
  req <- newXMLHttpRequest
  openSimple @_ @String req "GET" url
  send req
  Response <$> getResponseText req

