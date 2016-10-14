{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Session where
  import Control.Lens
  import Class.Name
  import Data.Type
  import Storage.Cache.Env
  import Storage.Db.Env

  data Session =
    Session {
      _sessionCache :: Cache,
      _sessionDbEnv :: DbEnv
    }

  class HasSession a where
    session :: Lens' a Session
    sessionCache :: Lens' a Cache
    sessionDbEnv :: Lens' a DbEnv

    sessionCache = session . sessionCache
    sessionDbEnv = session . sessionDbEnv

  instance HasSession Session where
    session = id

    sessionCache = lens _sessionCache (\s a -> s { _sessionCache = a })
    sessionDbEnv = lens _sessionDbEnv (\s a -> s { _sessionDbEnv = a })

  instance HasCache Session where
    cache = sessionCache . cache

  instance HasCacheSlot Session Name where
    slot _ = cache . cacheName

  instance HasCacheSlot Session (Type, Name) where
    slot _ = cache . cacheTypeName

  instance HasDbEnv Session where
    dbEnv = sessionDbEnv . dbEnv
