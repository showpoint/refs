{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Run where
  import Control.Monad.Logger
  import Control.Monad.Reader
  import Control.Lens
  import Database.Persist.Sqlite
  import Session
  import Storage.Cache.Env
  import Storage.Db
  import Storage.Db.Env
  import Storage.Db.Schema

  run connStr f = runNoLoggingT $ withSqliteConn connStr $ \conn -> do
    cache <- liftIO newCache
    runReaderT f $ Session cache (DbEnv conn)

  runInit connStr f = run connStr $ db (runMigration migrateAll) >> f
