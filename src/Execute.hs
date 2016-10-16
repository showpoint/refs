{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Execute where
  import Control.Monad.IO.Class

  class Monad m => Execute a m where
    execute :: Executable a m -> m ()

  data Executable a m = Execute a m => MkExecutable { unExecutable :: a }
