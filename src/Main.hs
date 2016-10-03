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

  type Ref a = Tagged a (Type, Name)

  data A = A {
      _nameA :: Name,
      _valuesA :: [Ref B]
    }
    deriving (Show)

  data B = B {
      _nameB :: Name,
      _valuesB :: [Ref B]
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

  class HasName a => HasCacheSlot a c where
    slot :: Ref a -> Lens' c (Cached a)

  instance HasCacheSlot A Cache where slot _ = cachedA
  instance HasCacheSlot B Cache where slot _ = cachedB

  insert a = ask >>= liftIO . flip modifyIORef' (slot (unproxy (const (typeOf a, nameOf a))) %~ Map.insert (nameOf a) a)

  lookup s n = do
    ref <- ask
    c <- liftIO $ readIORef ref
    let ca = c ^. s
    return $ Map.lookup n ca

  lookupA = lookup cachedA
  lookupB = lookup cachedB

  withCache m = newIORef (Cache Map.empty Map.empty) >>= runReaderT m

  instance HasCacheSlot a c => HasCacheSlot (Ref a) c where
    slot _ = slot (undefined :: a)

  instance HasType a => HasType (Ref a) where
    typeOf (Tagged (t, _)) = t

  instance HasName (Ref a) where
    nameOf (Tagged (_, n)) = n

  unref ref = lookup (slot ref) (nameOf ref)

  test = withCache $ do
    insert (A "a" [Tagged (ObjB, "a") :: Ref B, Tagged (ObjB, "b") :: Ref B])
    insert (B "a" [Tagged (ObjB, "b") :: Ref B, Tagged (ObjB, "a") :: Ref B])
    a <- lookupA "a"
    b <- lookupB "a"
    let tc = Tagged (ObjB, "b") :: Ref B
    c <- unref tc
    return (a, b, c, tc)

  main :: IO ()
  main = test >>= print
