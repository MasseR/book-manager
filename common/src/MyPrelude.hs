module MyPrelude
  ( module X
  , ByteString
  , LByteString
  , T.Text
  , tshow
  , putStrLn
  , putStr
  , void
  )
  where

import           Control.Monad.Reader as X
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Prelude              as X hiding (putStr, putStrLn, readFile)

putStrLn, putStr :: X.MonadIO m => T.Text -> m ()
putStrLn = X.liftIO . T.putStrLn
putStr = X.liftIO . T.putStr

type ByteString = B.ByteString
type LByteString = LB.ByteString

tshow :: Show a => a -> T.Text
tshow = T.pack . show
