{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Database.Internal
  ( SQL.Query(..)
  , SQL.Only(..)
  , SQL.ToRow(..)
  , SQL.ToField(..)
  , SQL.FromField(..)
  , HasConnection(..)
  , Connection
  , WithDB
  , execute_
  , execute
  , query
  , withConnection
  )
  where

import qualified Database.SQLite.Simple           as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Database.SQLite.Simple.ToField   as SQL

import           MyPrelude

import           Control.Lens                     (Lens', lens, set, view)

newtype Connection = Connection SQL.Connection

withConnection :: String -> (Connection -> IO a) -> IO a
withConnection path f = SQL.withConnection path (f . Connection)

withResource :: (MonadReader r m, HasConnection r) => (SQL.Connection -> m a) -> m a
withResource f = view connection >>= \(Connection c) -> f c

class HasConnection a where
  {-# MINIMAL getConnection, setConnection | connection #-}
  getConnection :: a -> Connection
  getConnection = view connection
  setConnection :: a -> Connection -> a
  setConnection = flip (set connection)
  connection :: Lens' a Connection
  connection = lens getConnection setConnection

type WithDB env m = (MonadReader env m, HasConnection env, MonadIO m)

execute_ :: WithDB env m => SQL.Query -> m ()
execute_ q = withResource $ \conn -> liftIO (SQL.execute_ conn q)

execute :: (WithDB env m, SQL.ToRow q) => SQL.Query -> q -> m ()
execute q params = withResource $ \conn -> liftIO (SQL.execute conn q params)

query
  :: (WithDB env m, SQL.ToRow q, SQL.FromRow r)
  => SQL.Query
  -> q
  -> m [r]
query q params = withResource $ \conn -> liftIO (SQL.query conn q params)
