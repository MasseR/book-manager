{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           JSDOM.Custom.XMLHttpRequest
import           Language.Javascript.JSaddle.Warp
import           MyPrelude

main :: IO ()
main = do
  let port = 8081
  putStrLn $ "Running on port " <> tshow port
  run port $ do
    putStrLn "Opening connection"
    req <- newXMLHttpRequest
    -- _ <- on req load $ do
    --   e <- ask >>= lift . fromJSVal @String . unXMLHttpRequestProgressEvent
    --   putStrLn (tshow e)
    openSimple @_ @String @String req "GET" "http://localhost:8088/version"
    send req
    getResponseText @_ @String req >>= putStrLn . tshow
    -- eventListenerNew _ _
    putStrLn "Opened connection"
    putStrLn "Hello world"
