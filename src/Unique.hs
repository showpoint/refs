{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
module Unique where
  class HasUnique a where
    type Unique a :: *
    unique :: a -> Unique a

  class HasUnique a => UnUnique a where
    ununique :: Unique a -> a
