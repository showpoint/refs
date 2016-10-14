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

  db m = view dbConn >>= liftIO . runSqlPersistM m

  find :: (PersistEntity (DbEntity a), MonadReader s m, HasDbEnv s, MonadIO m,
           HasDbRepr a (DbEntity a), HasDbKey a (DbEntity a)) => Ref a -> m (Maybe a)
  find ref = fmap (fromRepr . entityVal) <$> db (getBy (refToKey ref))

  store :: (PersistEntity (DbEntity a), MonadReader s m, HasDbEnv s, MonadIO m,
            HasDbRepr a (DbEntity a), HasDbKey a (DbEntity a), Show a) => a -> m ()
  store a = db $ deleteBy (key a) >> insert_ (toRepr a)
