{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving #-}
module Cactus.Clash.CPU
       ( CPU
       , input, output, abort
       , runCPU, runCPUDebug
       , HKD(..)

       , unG -- XXX
       ) where

import Clash.Prelude hiding (lift)
import Cactus.Clash.CPU.Internal
import Control.Monad.State hiding (state)
import GHC.Generics
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

newtype CPU i s o a = CPU{ unCPU :: ExceptT () (RWS i (G (o Last)) s) a }
    deriving newtype (Functor)
deriving newtype instance (Generic (o Last), GMonoid (Rep (o Last))) => Applicative (CPU i s o)
deriving newtype instance (Generic (o Last), GMonoid (Rep (o Last))) => Monad (CPU i s o)
deriving newtype instance (Generic (o Last), GMonoid (Rep (o Last))) => MonadState s (CPU i s o)

input :: (Generic (o Last), GMonoid (Rep (o Last))) => CPU i s o i
input = CPU ask

output :: (Generic (o Last), GMonoid (Rep (o Last))) => o Last -> CPU i s o ()
output = CPU . tell . G

abort :: (Generic (o Last), GMonoid (Rep (o Last))) => CPU i s o a
abort = CPU $ throwE ()

runCPU
  :: (Generic (o Identity), Generic (o Last), GUpdate (Rep (o Identity)) (Rep (o Last)))
  => (s -> o Identity) -> CPU i s o () -> (i -> State s (o Identity))
runCPU mkDef cpu inp = do
    s <- get
    let (s', writes) = execRWS (runExceptT $ unCPU cpu) inp s
    put s'
    def <- gets mkDef
    return $ update def (unG writes)

runCPUDebug
  :: (Generic (o Identity), Generic (o Last), GUpdate (Rep (o Identity)) (Rep (o Last)))
  => (s -> o Identity) -> CPU i s o () -> (i -> State s (s, o Identity))
runCPUDebug mkDef cpu inp = do
    s0 <- get
    out <- runCPU mkDef cpu inp
    return (s0, out)
