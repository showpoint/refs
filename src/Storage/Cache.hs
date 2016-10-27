{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Storage.Cache where
  import Control.Lens
  import Control.Monad.Reader
  import Data.IORef
  import Data.Dynamic hiding (typeOf)
  import qualified Data.Map as Map
  import Data.Map (Map)
  import Debug.Trace
  import Class.Name
  import Data.Type
  import Refs
  import Storage.Cache.Class
  import Storage.Cache.Env

  find :: (Typeable a, MonadIO m, MonadReader c m, HasCache c, HasCacheSlot c (CacheKey a)) => Ref a -> m (Maybe a)
  find (Ref u) = do
    c <- view (slot u) >>= liftIO . readIORef
    return $ fromDynamic =<< Map.lookup u c

  store :: (Typeable a, MonadIO m, MonadReader c m, HasCache c, HasCacheKey a, HasCacheSlot c (CacheKey a), Show a) => a -> m ()
  store a = view (slot $ unique a) >>= liftIO . flip modifyIORef (Map.insert (unique a) (toDyn a))
