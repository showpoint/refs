{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Storage.Cache.Class where
  class Ord (CacheKey a) => HasCacheKey a where
    type CacheKey a :: *
    unique :: a -> CacheKey a
