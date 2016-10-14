{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module Main where
  import Debug.Trace
  import Control.Monad.State
  import Data.Dynamic hiding (typeOf)
  import qualified Data.Map as Map
  import Data.Map (Map)
  import Data
  import Refs
  import Storage
  import Storage.Db
  import Run

  test = do
    let ra = ref (A "A") :: Ref A
        rb = ref (B "B") :: Ref B
        rc = ref (C "B") :: Ref C
    store $ A "A"
    store $ A "AA"
    store $ C "A"
    store $ C "B"
    store $ A "B"
    traceM $ show ra
    find ra >>= traceM . show
    traceM $ show rb
    --find rb >>= traceM . show
    traceM $ show rc
    find rc >>= traceM . show

  main :: IO ()
  main = runInit test >>= print
