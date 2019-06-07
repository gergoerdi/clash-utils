{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving #-}
module Cactus.Clash.CPU
       ( CPU
       , input, output, abort
       , runCPU, runCPUDebug
       ) where

import Clash.Prelude hiding (lift)
import Control.Monad.State hiding (state)
import GHC.Generics
import Data.Generic.HKD
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Barbie

newtype CPU i s o a = CPU{ unCPU :: ExceptT () (RWS i (HKD o Last) s) a }
    deriving newtype (Functor)
deriving newtype instance (Monoid (HKD o Last)) => Applicative (CPU i s o)
deriving newtype instance (Monoid (HKD o Last)) => Monad (CPU i s o)
deriving newtype instance (Monoid (HKD o Last)) => MonadState s (CPU i s o)

input :: (Monoid (HKD o Last)) => CPU i s o i
input = CPU ask

output :: (Monoid (HKD o Last)) => HKD o Last -> CPU i s o ()
output = CPU . tell

abort :: (Monoid (HKD o Last)) => CPU i s o a
abort = CPU $ throwE ()

runCPU
  :: (Generic o, Construct Identity o, FunctorB (HKD o), ProductBC (HKD o))
  => (s -> o) -> CPU i s o () -> (i -> State s o)
runCPU mkDef cpu inp = do
    s <- get
    let (s', writes) = execRWS (runExceptT $ unCPU cpu) inp s
    put s'
    def <- gets mkDef
    return $ update def writes

runCPUDebug
  :: (Generic o, Construct Identity o, FunctorB (HKD o), ProductBC (HKD o))
  => (s -> o) -> CPU i s o () -> (i -> State s (s, o))
runCPUDebug mkDef cpu inp = do
    s0 <- get
    out <- runCPU mkDef cpu inp
    return (s0, out)

update :: (Generic a, Construct Identity a, FunctorB (HKD a), ProductBC (HKD a)) => a -> HKD a Last -> a
update initial edits = runIdentity . construct $ bzipWith (\i -> maybe i Identity . getLast) (deconstruct initial) edits
