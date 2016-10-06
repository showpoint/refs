{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Main where
  import Data.Proxy
  import Data.Tagged hiding (proxy)

  data A = A Name deriving Show
  data B = B Name deriving Show

  type Name = String
  class HasName a where
    nameOf :: a -> Name

  instance HasName A where nameOf (A n) = n
  instance HasName B where nameOf (B n) = n

  data Type = TA | TB deriving Show

  class HasType a where
    type TypeOf a
    typeOf :: a -> TypeOf a
    typeOf _ = typeProxy (Proxy :: Proxy a)
    typeProxy :: Proxy a -> TypeOf a

  instance HasType A where { type TypeOf A = Type; typeProxy _ = TA }
  instance HasType B where { type TypeOf B = Type; typeProxy _ = TB }

  data Ref a = Ref { unRef :: RefData a }

  instance (Show r, r ~ RefData a) => Show (RefData a) where

  class Show (RefData a) => HasRef a r | a -> r where
    type RefData a :: *
    ref      :: a -> Ref a
    refProxy :: Proxy a -> RefData a -> Ref a
    refProxy _ = Ref
    rebuild  :: Ref a -> a

  instance HasRef A (Name, Type) where
    type RefData A = (Name, Type)
    ref a = Ref (nameOf a, typeOf a)
    rebuild = A . fst . unRef

  refA = refProxy (Proxy :: Proxy A)

  instance HasRef B Name where
    type RefData B = Name
    ref = Ref . nameOf
    rebuild = B . unRef

  main :: IO ()
  main = do
    let a = ref (A "A") :: Ref A
        b = ref (B "B") :: Ref B
--    print a
    print $ rebuild a
--    print b
    print $ rebuild b
