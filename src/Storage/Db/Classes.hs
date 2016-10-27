{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Storage.Db.Classes where
  import Database.Persist
  import Refs

  class HasDbEntity a where
    type DbEntity a :: *

  class HasDbKey a where
    key :: a -> Unique (DbEntity a)
    refToKey :: Ref a -> Unique (DbEntity a)

  class HasDbEntity a => HasDbRepr a where
    fromRepr :: DbEntity a -> a
    toRepr :: a -> DbEntity a
