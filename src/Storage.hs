{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Storage where
  import Data.Dynamic hiding (typeOf)
  import qualified Data.Map as Map
  import Data.Map (Map)
  import Refs

  class Monad m => Find a m where
    find :: Ref a -> m (Maybe a)

  class Monad m => Store a m where
    store :: a -> m ()
