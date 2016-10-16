{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Refs where
  import Data.Serialize
  import GHC.Generics
  import Data.Proxy
  import Unique

  data Ref a = Ref { unRef :: Unique a } deriving (Generic)

  instance Show (Unique a) => Show (Ref a) where
    showsPrec d (Ref u) = showParen (d > 10) $ showString "Ref " . showsPrec 11 u

  instance Serialize (Unique a) => Serialize (Ref a)

  class (HasUnique a, Show (Unique a)) => HasRef a where
    ref :: a -> Ref a
    ref = Ref . unique
    refProxy :: Proxy a -> Unique a -> Ref a
    refProxy _ = Ref
