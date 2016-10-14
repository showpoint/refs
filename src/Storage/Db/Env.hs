module Storage.Db.Env where
  import Database.Persist.Sql
  import Control.Lens

  data DbEnv
    = DbEnv {
      _dbConn :: SqlBackend
    }

  class HasDbEnv t where
    dbEnv :: Lens' t DbEnv
    dbConn :: Lens' t SqlBackend

    dbConn = dbEnv . dbConn

  instance HasDbEnv DbEnv where
    dbEnv = id

    dbConn = lens _dbConn $ \s a -> s { _dbConn = a }
