{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Run where
  import Control.Monad.Logger
  import Control.Monad.Reader
  import Control.Lens
  import Database.Persist.Sqlite
  import Storage.Db
  import Storage.Db.Env
  import Storage.Db.Schema

  run f = runNoLoggingT $ withSqliteConn "D:\\Work\\Haskell\\refs\\refs.db" $ \conn ->
    runReaderT f (DbEnv conn)

  runInit f = run $ db (runMigration migrateAll) >> f
