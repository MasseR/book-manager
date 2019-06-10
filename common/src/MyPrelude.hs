module MyPrelude
  ( module X
  , Generic
  , ByteString
  , LByteString
  , T.Text
  , tshow
  , putStrLn
  , putStr
  , print
  , hush
  , ifM
  , boolM
  , whenM
  , unlessM
  )
  where

import           Control.Monad.Reader as X
import           Data.Bifunctor       as X
import           Data.Bool            as X (bool)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe           as X (catMaybes, fromMaybe, isJust, maybe)
import           Data.Profunctor      as X
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           GHC.Generics         (Generic)
import           Prelude              as X hiding (print, putStr, putStrLn,
                                            readFile)
import           System.Directory     as X (getDirectoryContents)
import           System.FilePath      as X
import           Text.Read            as X (readMaybe)

putStrLn, putStr :: X.MonadIO m => T.Text -> m ()
putStrLn = X.liftIO . T.putStrLn
putStr = X.liftIO . T.putStr

print :: (X.MonadIO m, Show a) => a -> m ()
print = putStrLn . tshow


type ByteString = B.ByteString
type LByteString = LB.ByteString

tshow :: Show a => a -> T.Text
tshow = T.pack . show

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

boolM :: Monad m => m a -> m a -> m Bool -> m a
boolM f t = bool f t <=< id

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do
  b' <- b
  if b' then t else f

whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (pure ())

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b = ifM b (pure ())

