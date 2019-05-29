module MyPrelude
  ( module X
  , ByteString
  , LByteString
  , T.Text
  , tshow
  )
  where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import           Prelude              as X

type ByteString = B.ByteString
type LByteString = LB.ByteString

tshow :: Show a => a -> T.Text
tshow = T.pack . show
