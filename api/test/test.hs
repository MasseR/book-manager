module Main where

import           Test.Hspec

import qualified Test.API as API

main :: IO ()
main = hspec $
  API.spec
