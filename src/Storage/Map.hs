{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Storage.Map where
  import Control.Monad.State
  import Data.Dynamic hiding (typeOf)
  import qualified Data.Map as Map
  import Data.Map (Map)
  import Unique
  import Data
  import Refs
  import Storage

  type Corned a = Map (Unique a) Dynamic

  type MonadCorn a m = MonadState (Corned a) m

  instance (Monad m, MonadIO m, MonadCorn a m, Ord (Unique a), HasRef a, Typeable a) => Find a m where
    find (Ref u) = maybe Nothing fromDynamic <$> gets (Map.lookup u)

  instance (Monad m, MonadIO m, MonadCorn a m, Ord (Unique a), HasUnique a, Typeable a) => Store a m where
    store a = modify (Map.insert (unique a) (toDyn a))
