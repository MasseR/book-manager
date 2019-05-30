{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
module DB.Internal
  ( SQL.Query(..)
  , SQL.Only(..)
  , SQL.Connection
  , SQL.withConnection
  , SQL.ToRow(..)
  , SQL.ToField(..)
  , HasDB(..)
  , WithDB
  , execute_
  , execute
  , query )
  where

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL

import MyPrelude

class HasDB a where
  getConnection :: a -> SQL.Connection

type WithDB env m = (MonadReader env m, HasDB env, MonadIO m)

execute_ :: WithDB env m => SQL.Query -> m ()
execute_ q = asks getConnection >>= \conn -> liftIO (SQL.execute_ conn q)

execute :: (WithDB env m, SQL.ToRow q) => SQL.Query -> q -> m ()
execute q params = asks getConnection >>= \conn -> liftIO (SQL.execute conn q params)

query
  :: (WithDB env m, SQL.ToRow q, SQL.FromRow r)
  => SQL.Query
  -> q
  -> m [r]
query q params = asks getConnection >>= \conn -> liftIO (SQL.query conn q params)
