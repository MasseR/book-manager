{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           DB.Internal                 (withConnection)
import           DB.Migrations
import           MyPrelude
import           Server
import           Types

import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors
import           Servant.Auth.Server         (generateKey)

main :: IO ()
main = withConnection "/tmp/book-manager.sqlite" $ \connection -> do
  jwtKey <- generateKey
  let port = 8088
      app = App { connection
                , jwtKey }

  runReaderT (runMigration Nothing) app

  putStrLn $ "Running on port " <> tshow port
  run port (simpleCors $ application app)
