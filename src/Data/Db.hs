{-# LANGUAGE TemplateHaskell #-}
module Data.Db where
  import Database.Persist.TH
  import Data.Type

  derivePersistField "Type"
