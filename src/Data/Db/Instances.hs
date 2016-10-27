{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Db.Instances where
  import Database.Persist (PersistEntity)
  import Data.Serialize
  import Class.Name
  import Class.Type
  import Data
  import Storage.Db.Classes
  import Storage.Db.Schema
  import Refs
  import Unique

  instance HasDbEntity A where type DbEntity A = Data
  instance HasDbEntity B where type DbEntity B = Bs
  instance HasDbEntity C where type DbEntity C = Data

  instance HasDbKey A where
    key = uncurry DataTypeNameUniq . unique
    refToKey = uncurry DataTypeNameUniq . unRef

  instance HasDbKey B where
    key = BsNameUniq . unique
    refToKey = BsNameUniq . unRef

  instance HasDbKey C where
    key = uncurry DataTypeNameUniq . unique
    refToKey = uncurry DataTypeNameUniq . unRef

  instance HasDbRepr A where
    fromRepr (Data _ _ d) = either (error "DecodeError") id $ decode d
    toRepr a = Data (typeOf a) (nameOf a) $ encode a

  instance HasDbRepr B where
    fromRepr (Bs _ d) = either (error "DecodeError") id $ decode d
    toRepr a = Bs (nameOf a) $ encode a

  instance HasDbRepr C where
    fromRepr (Data _ _ d) = either (error "DecodeError") id $ decode d
    toRepr a = Data (typeOf a) (nameOf a) $ encode a
