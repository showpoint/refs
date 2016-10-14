{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
module Storage.Db.Schema where
  import Database.Persist
  import Database.Persist.TH
  import Data.ByteString
  import Class.Name
  import Data
  import Data.Db

  share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Data
      type Type
      name Name
      data ByteString
      DataTypeNameUniq type name
  |]
