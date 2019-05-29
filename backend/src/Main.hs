module Main where

import           Server
import           Types

import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors

main :: IO ()
main = do
  let port = 8088
      app = App
  putStrLn $ "Running on port " <> show port
  run port (simpleCors $ application app)
