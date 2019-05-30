{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           DB.Migrations
import DB.Internal (withConnection)
import           MyPrelude
import           Server
import           Types

import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors

main :: IO ()
main = withConnection "/tmp/book-manager.sqlite" $ \connection -> do
  let port = 8088
      app = App {connection}

  runReaderT (runMigration Nothing) app

  putStrLn $ "Running on port " <> tshow port
  run port (simpleCors $ application app)
