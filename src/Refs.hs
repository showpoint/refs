{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Refs where
  import Data.Proxy
  import Unique

  data Ref a = Show (Unique a) => Ref { unRef :: Unique a }

  deriving instance Show (Ref a)

  class (HasUnique a, Show (Unique a)) => HasRef a where
    ref :: a -> Ref a
    ref = Ref . unique
    refProxy :: Proxy a -> Unique a -> Ref a
    refProxy _ = Ref
