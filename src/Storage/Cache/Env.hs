{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Storage.Cache.Env where
  import Control.Monad.IO.Class
  import Data.Dynamic
  import Data.IORef
  import Data.Map (Map, empty)
  import Control.Lens
  import Class.Name
  import Class.Type
  import Data.Type

  type Cached k = IORef (Map k Dynamic)

  data Cache
    = Cache {
      _cacheName     :: Cached Name,
      _cacheTypeName :: Cached (Type, Name)
    }

  newCache = Cache <$> newIORef empty <*> newIORef empty

  class HasCache t where
    cache :: Lens' t Cache
    cacheName :: Lens' t (Cached Name)
    cacheTypeName :: Lens' t (Cached (Type, Name))

    cacheName = cache . cacheName
    cacheTypeName = cache . cacheTypeName

  instance HasCache Cache where
    cache = id

    cacheName = lens _cacheName $ \s a -> s { _cacheName = a }
    cacheTypeName = lens _cacheTypeName $ \s a -> s { _cacheTypeName = a }

  class HasCache t => HasCacheSlot t k where
    slot :: k -> Lens' t (Cached k)

  instance HasCacheSlot Cache Name where
    slot _ = cacheName

  instance HasCacheSlot Cache (Type, Name) where
    slot _ = cacheTypeName
