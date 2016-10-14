{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
  import Data.Type
  import Refs
  import Run
  import Storage

  test = do
    find (refA (TA, "A")) >>= traceM . show
    find (refB "B")       >>= traceM . show
    find (refC (TC, "A")) >>= traceM . show

  main :: IO ()
  main = runInit "D:\\Work\\refs\\refs.db" test >>= print
