{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Storage where
  import Debug.Trace
  import qualified Storage.Cache as Cache
  import qualified Storage.Db as Db

  find r = do
    ma <- Cache.find r
    case ma of
      Just a -> return (Just a)
      Nothing -> do
        ma <- Db.find r
        case ma of
          Just a -> do
            Cache.store a
            return (Just a)
          Nothing -> return Nothing

  store a = do
    Cache.store a
    Db.store a
