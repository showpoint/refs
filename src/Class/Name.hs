{-# LANGUAGE NoMonomorphismRestriction #-}
module Class.Name where
  type Name = String

  class HasName a where
    nameOf :: a -> Name
