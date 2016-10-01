{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where
  import Prelude hiding (lookup)
  import Control.Lens
  import Control.Monad.IO.Class
  import Control.Monad.Reader
  import Data.IORef
  import qualified Data.Map as Map
  import Data.Map.Strict (Map)
  import Data.Monoid ((<>))
  import Data.Tagged

  type Name = String

  data A = A {
      _nameA :: Name,
      _refB :: Tagged B Name
    }
    deriving (Show)

  data B = B {
      _nameB :: Name,
      _valueB :: Char
    }
    deriving (Show)

  data Type
    = ObjA
    | ObjB
    deriving (Eq, Ord, Show)

  class HasName a where nameOf :: a -> Name
  instance HasName A where nameOf = _nameA
  instance HasName B where nameOf = _nameB

  class HasType a where typeOf :: a -> Type
  instance HasType A where typeOf _ = ObjA
  instance HasType B where typeOf _ = ObjB

  type Cached a = Map Name a

  data Cache
    = Cache {
      _cachedA :: Cached A,
      _cachedB :: Cached B
    }
    deriving (Show)

  cachedA = lens _cachedA (\s a -> s { _cachedA = a })
  cachedB = lens _cachedB (\s a -> s { _cachedB = a })

  class HasName a => CanBeCached a c where
    slot :: Tagged a Name -> Lens' c (Cached a)

  instance CanBeCached A Cache where
    slot _ = cachedA

  instance CanBeCached B Cache where
    slot _ = cachedB

  insert a = ask >>= liftIO . flip modifyIORef' (slot (unproxy (const (nameOf a))) %~ Map.insert (nameOf a) a)

  lookup s n = do
    ref <- ask
    c <- liftIO $ readIORef ref
    let ca = c ^. s
    return $ Map.lookup n ca

  lookupA = lookup cachedA
  lookupB = lookup cachedB

  withCache m = newIORef (Cache Map.empty Map.empty) >>= runReaderT m

  instance CanBeCached a c => CanBeCached (Tagged a Name) c where
    slot _ = slot (undefined :: a)

  instance HasName (Tagged a Name) where
    nameOf = untag

  unref tagged = lookup (slot tagged) (nameOf tagged)

  test = withCache $ do
    insert (A "a" (Tagged "a" :: Tagged B Name))
    insert (B "a" 'c')
    a <- lookupA "a"
    b <- lookupB "a"
    let tc = Tagged "a" :: Tagged B Name
    c <- unref tc
    return (a, b, c, tc)

  main :: IO ()
  main = test >>= print
