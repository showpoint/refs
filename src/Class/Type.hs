{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Class.Type where
  class (Eq t, Ord t, Show t) => HasType a t | a -> t where
    typeOf :: a -> t
