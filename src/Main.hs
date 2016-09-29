{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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
  import Data.Proxy

  type Name = String

  data A = A {
      _nameA :: Name,
      _valueA :: Int
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

  class HasName a where
    nameOf :: a -> Name

  instance HasName A where nameOf = _nameA
  instance HasName B where nameOf = _nameB

  class HasValue v a | a -> v where
    valueOf :: a -> v

  instance HasValue Int A where valueOf = _valueA
  instance HasValue Char B where valueOf = _valueB

  class HasType a where
    typeOf :: a -> Type

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

  class HasCacheSlot a c where
    slot :: a -> Lens' c (Cached a)

  instance HasCacheSlot A Cache where slot _ = cachedA
  instance HasCacheSlot B Cache where slot _ = cachedB

  insert a = ask >>= liftIO . flip modifyIORef' (slot a %~ Map.insert (nameOf a) a)

  lookup s n = do
    ref <- ask
    c <- liftIO $ readIORef ref
    let ca = c ^. s
    return $ Map.lookup n ca

  lookupA = lookup cachedA
  lookupB = lookup cachedB

  withCache m = newIORef (Cache Map.empty Map.empty) >>= runReaderT m

  test = withCache $ do
    insert (A "a" 1)
    insert (B "a" 'c')
    a <- lookupA "a"
    b <- lookupB "a"
    return (a, b)

  main :: IO ()
  main = test >>= print
