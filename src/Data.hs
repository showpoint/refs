{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Data where
  import Control.Monad.IO.Class
  import Data.Serialize
  import Data.Typeable hiding (typeOf)
  import GHC.Generics (Generic)
  import Class.Name
  import Class.Type
  import Data.Type
  import Unique
  import Refs
  import Execute

  data A = A Name (Ref B) deriving (Show, Typeable, Generic, Serialize)
  data B = B Name deriving (Show, Typeable, Generic, Serialize)
  data C = C Name deriving (Show, Typeable, Generic, Serialize)

  instance HasName A where nameOf (A n _) = n
  instance HasName B where nameOf (B n) = n
  instance HasName C where nameOf (C n) = n

  instance HasType A Type where typeOf _ = TA
  instance HasType B Type where typeOf _ = TB
  instance HasType C Type where typeOf _ = TC

  instance HasUnique A where type Unique A = (Type, Name); unique a = (typeOf a, nameOf a)
  instance HasUnique B where type Unique B = Name;         unique = nameOf
  instance HasUnique C where type Unique C = (Type, Name); unique a = (typeOf a, nameOf a)

  instance UnUnique A where ununique = flip A (Ref "A") . snd
  instance UnUnique B where ununique = B
  instance UnUnique C where ununique = C . snd

  instance HasRef A
  instance HasRef B
  instance HasRef C

  refA = refProxy (Proxy :: Proxy A) . (TA,)
  refB = refProxy (Proxy :: Proxy B)
  refC = refProxy (Proxy :: Proxy C) . (TC,)

  instance MonadIO m => Execute A m where
    execute = liftIO . print . unExecutable

  instance MonadIO m => Execute B m where
    execute = liftIO . print . unExecutable

  instance MonadIO m => Execute C m where
    execute _ = liftIO $ putStrLn "This is C"
