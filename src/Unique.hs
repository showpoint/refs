{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Unique where
  import Data.Serialize
  import GHC.Generics

  class HasUnique a where
    type Unique a :: *
    unique :: a -> Unique a

  class HasUnique a => UnUnique a where
    ununique :: Unique a -> a
