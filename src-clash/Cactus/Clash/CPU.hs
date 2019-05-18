{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Cactus.Clash.CPU where

import Clash.Prelude hiding (lift)
import Control.Monad.State hiding (state)
import Control.Monad.Writer as W
import Control.Monad.RWS
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Monoid

newtype CPU i s o a = CPU{ unCPU :: ExceptT () (RWS i (Endo o) s) a }
    deriving newtype (Functor, Applicative, Monad, MonadState s)

tell :: (o -> o) -> CPU i s o ()
tell = CPU . W.tell . Endo

input :: CPU i s o i
input = CPU ask

abort :: CPU i s o a
abort = CPU $ throwE ()

runCPU :: (s -> o) -> CPU i s o () -> (i -> State s o)
runCPU mkDef cpu inp = do
    s <- get
    let (s', f) = execRWS (runExceptT $ unCPU cpu) inp s
    put s'
    def <- gets mkDef
    return $ appEndo f def

runCPUDebug :: (s -> o) -> CPU i s o () -> (i -> State s (s, o))
runCPUDebug mkDef cpu inp = do
    s0 <- get
    out <- runCPU mkDef cpu inp
    return (s0, out)
