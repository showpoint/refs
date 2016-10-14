{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Storage.Db.Classes where
  import Data.Proxy
  import qualified Database.Persist as Db
  import Database.Persist (PersistEntity, PersistEntityBackend)
  import Database.Persist.Sql (SqlBackend)
  import Refs
  import Unique

  class (DbEntity a ~ t, PersistEntityBackend t ~ SqlBackend, PersistEntity (DbEntity a)) => HasDbEntity a t | a -> t where
    type DbEntity a :: *

  class HasDbKey a t | a -> t where
    key :: a -> Db.Unique (DbEntity a)
    refToKey :: Ref a -> Db.Unique (DbEntity a)

  class (DbEntity a ~ t, HasDbEntity a t) => HasDbRepr a t where
    fromRepr :: t -> a
    toRepr :: a -> t
