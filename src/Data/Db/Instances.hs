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

  instance HasDbEntity A Data where type DbEntity A = Data
  instance HasDbEntity B Bs   where type DbEntity B = Bs
  instance HasDbEntity C Data where type DbEntity C = Data

  instance HasDbKey A Data where
    key = uncurry DataTypeNameUniq . unique
    refToKey = uncurry DataTypeNameUniq . unRef

  instance HasDbKey B Bs where
    key = BsNameUniq . unique
    refToKey = BsNameUniq . unRef

  instance HasDbKey C Data where
    key = uncurry DataTypeNameUniq . unique
    refToKey = uncurry DataTypeNameUniq . unRef

  instance HasDbRepr A Data where
    fromRepr (Data _ _ d) = either (error "DecodeError") id $ decode d
    toRepr a = Data (typeOf a) (nameOf a) $ encode a

  instance HasDbRepr B Bs where
    fromRepr (Bs _ d) = either (error "DecodeError") id $ decode d
    toRepr a = Bs (nameOf a) $ encode a

  instance HasDbRepr C Data where
    fromRepr (Data _ _ d) = either (error "DecodeError") id $ decode d
    toRepr a = Data (typeOf a) (nameOf a) $ encode a
