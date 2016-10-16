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
  import Data.Maybe
  import qualified Data.Map as Map
  import Data.Map (Map)
  import Data
  import Data.Type
  import Execute
  import Refs
  import Run
  import Storage

  b (A _ b') = b'

  test = do
    store $ A "AA" (Ref "B")
    a <- find (refA "AA")
    b <- find $ b (fromJust a)
    find (refA "AA") >>= execute . MkExecutable . fromJust
    find (refB "B") >>= execute . MkExecutable . fromJust
    find (refC "A") >>= execute . MkExecutable . fromJust

  main :: IO ()
  main = runInit "D:\\Work\\Haskell\\refs\\refs.db" test >>= print
