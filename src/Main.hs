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
  data C = C Name deriving Show

  type Name = String
  class HasName a where
    nameOf :: a -> Name

  instance HasName A where nameOf (A n) = n
  instance HasName B where nameOf (B n) = n
  instance HasName C where nameOf (C n) = n

  data Type = TA | TB | TC deriving Show

  class Show (TypeOf a) => HasType a where
    type TypeOf a
    typeOf :: a -> TypeOf a
    typeOf _ = typeProxy (Proxy :: Proxy a)
    typeProxy :: Proxy a -> TypeOf a

  instance HasType A where { type TypeOf A = Type; typeProxy _ = TA }
  instance HasType B where { type TypeOf B = Type; typeProxy _ = TB }
  instance HasType C where { type TypeOf C = Type; typeProxy _ = TC }

  type Ref a = Tagged a (RefData a)

  newtype TN a = TN { unTN :: (TypeOf a, Name) }

  instance HasType a => Show (TN a) where
    show (TN (t, n)) = show t ++ " " ++ show n

  class HasRef a r | a -> r where
    type RefData a :: *
    ref :: a -> Ref a
    refProxy :: Proxy a -> RefData a -> Ref a
    refProxy _ = Tagged
    rebuild :: Ref a -> a

  instance HasRef A (TN A) where
    type RefData A = TN A
    ref a = Tagged $ TN (typeOf a, nameOf a)
    rebuild = A . snd . unTN . untag

  refA = refProxy (Proxy :: Proxy A)

  instance HasRef B Name where
    type RefData B = Name
    ref = Tagged . nameOf
    rebuild = B . untag

  instance HasRef C (TN C) where
    type RefData C = TN C
    ref a = Tagged $ TN (typeOf a, nameOf a)
    rebuild = C . snd . unTN . untag

  main :: IO ()
  main = do
    let a = ref (A "A") :: Ref A
        b = ref (B "B") :: Ref B
        c = ref (C "B") :: Ref C
    print a
    print $ rebuild a
    print b
    print $ rebuild b
    print c
    print $ rebuild c
