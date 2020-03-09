{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
module DB.Migrations where

import           Control.Exception
import           Control.Monad     (when)
import qualified Data.Text.IO      as T
import           Text.Read         (readMaybe)

import           DB.Internal
import           MyPrelude         hiding (try)


import           System.Directory  (getDirectoryContents)
import           System.FilePath   (takeExtension, takeFileName, (</>))

data Migration = Migration { version   :: Int
                           , migration :: Query }
               deriving Show

-- XXX: Testing candidate
parseIdx :: FilePath -> Maybe Int
parseIdx = readMaybe . takeWhile (/= '_') . takeFileName

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

getQuery :: MonadIO m => FilePath -> m (Maybe Migration)
getQuery path = liftIO $ do
  sql <- try @IOException (Query <$> T.readFile path)
  return (Migration <$> parseIdx path <*> hush sql)

findMigrations :: MonadIO m => FilePath -> m [Migration]
findMigrations migrationsPath = liftIO $ do
  files <- filter sqlFile <$> directoryContents migrationsPath
  catMaybes <$> mapM getQuery files
  where
    directoryContents p = map (p </>) <$> getDirectoryContents p
    sqlFile f = takeExtension f == ".sql"

runMigration :: WithDB env m => Maybe FilePath -> m ()
runMigration path = do
  migrations <- findMigrations (fromMaybe "migrations" path)
  execute_ "create table if not exists migrations (migration_idx int primary key, updated timestamp default current_timestamp)"
  mapM_ migrate migrations
  where
    migrate Migration{..} = do
      rs <- query "select migration_idx from migrations where migration_idx = ?" (Only version)
      when (null (rs :: [Only Int])) $ do
        execute_ migration
        execute "insert into migrations (migration_idx) values (?)" (Only version)
