{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeApplications           #-}
module Network.Client.HTTP where

import           Control.Lens
import           Data.Text.Encoding          (encodeUtf8)
import           GHC.Generics                (Generic)
import           JSDOM.Custom.XMLHttpRequest
import           JSDOM.Types                 (MonadDOM)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B

import           MyPrelude


data Response a =
  Response { content :: Maybe a
           , status  :: Word }
                deriving (Generic, Functor, Show)

responseBody :: Monoid a => Lens' (Response a) a
responseBody = lens (fromMaybe mempty . content) (\r x -> r{content=Just x})

responseStatus :: Lens' (Response a) Word
responseStatus = lens status (\r x -> r{status = x})

get' :: (MonadDOM m) => String -> String -> String -> m (Response ByteString)
get' url username password = do
  let auth = "Basic " <> B64.encode (B.pack username <> ":" <> B.pack password)
  req <- newXMLHttpRequest
  openSimple @_ @String req "GET" url
  setRequestHeader @_ @String req "Authorization" (B.unpack auth)
  send req
  Response
    <$> (fmap encodeUtf8 <$> getResponseText req)
    <*> getStatus req

get :: MonadDOM m => String -> m (Response ByteString)
get url = do
  req <- newXMLHttpRequest
  openSimple @_ @String req "GET" url
  send req
  Response
    <$> (fmap encodeUtf8 <$> getResponseText req)
    <*> getStatus req

