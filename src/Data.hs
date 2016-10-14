{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
module Data where
  import Data.Serialize
  import Data.Typeable hiding (typeOf)
  import GHC.Generics (Generic)
  import Class.Name
  import Class.Type
  import Unique
  import Refs

  data A = A Name deriving (Show, Typeable, Generic, Serialize)
  data B = B Name deriving (Show, Typeable, Generic, Serialize)
  data C = C Name deriving (Show, Typeable, Generic, Serialize)

  instance HasName A where nameOf (A n) = n
  instance HasName B where nameOf (B n) = n
  instance HasName C where nameOf (C n) = n

  data Type = TA | TB | TC deriving (Eq, Ord, Show, Read)

  instance HasType A Type where typeOf _ = TA
  instance HasType B Type where typeOf _ = TB
  instance HasType C Type where typeOf _ = TC

  instance HasUnique A where type Unique A = (Type, Name); unique a = (typeOf a, nameOf a)
  instance HasUnique B where type Unique B = Name;         unique = nameOf
  instance HasUnique C where type Unique C = (Type, Name); unique a = (typeOf a, nameOf a)

  instance UnUnique A where ununique = A . snd
  instance UnUnique B where ununique = B
  instance UnUnique C where ununique = C . snd

  instance HasRef A
  instance HasRef B
  instance HasRef C
