{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Storage.Db where
  import Control.Lens
  import Control.Monad.IO.Class
  import Control.Monad.Reader
  import Data.Proxy
  import Data.Serialize
  import Debug.Trace
  import Database.Persist.Sql
  import Data.Db.Instances
  import Refs
  import Storage.Db.Classes
  import Storage.Db.Env
  import Unique

  type Persistable a = (HasDbRepr a, HasDbKey a, PersistEntity (DbEntity a), PersistEntityBackend (DbEntity a) ~ SqlBackend)
  type Db s m = (MonadIO m, MonadReader s m, HasDbEnv s)

  db m = view dbConn >>= liftIO . runSqlPersistM m

  find :: (Persistable a, Db s m) => Ref a -> m (Maybe a)
  find ref = fmap (fromRepr . entityVal) <$> db (getBy (refToKey ref))

  store :: (Persistable a, Db s m) => a -> m ()
  store a = db $ deleteBy (key a) >> insert_ (toRepr a)
