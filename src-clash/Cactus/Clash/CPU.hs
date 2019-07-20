{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Cactus.Clash.CPU
       ( CPU
       , input, inputs, output, abort
       , getStart, getsStart
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
import Control.Lens (Lens, (&), (.~))
import GHC.OverloadedLabels
import Data.Generics.Product.Fields (HasField')

type Partial o = HKD o Last

newtype CPU i s o a = CPU{ unCPU :: ExceptT () (RWS (i, s) (Partial o) s) a }
    deriving newtype (Functor)
deriving newtype instance (Monoid (Partial o)) => Applicative (CPU i s o)
deriving newtype instance (Monoid (Partial o)) => Monad (CPU i s o)
deriving newtype instance (Monoid (Partial o)) => MonadState s (CPU i s o)

instance (HasField' field (HKD a f) (f b), Applicative f) => IsLabel field (b -> Endo (HKD a f)) where
    {-# INLINE fromLabel #-}
    fromLabel x = Endo $ field @field .~ pure x

input :: (Monoid (Partial o)) => CPU i s o i
input = inputs id

inputs :: (Monoid (Partial o)) => (i -> a) -> CPU i s o a
inputs f = CPU . asks $ f . fst

getStart :: (Monoid (Partial o)) => CPU i s o s
getStart = getsStart id

getsStart :: (Monoid (Partial o)) => (s -> a) -> CPU i s o a
getsStart f = CPU . asks $ f . snd

{-# INLINE output_ #-}
output_ :: (Monoid (Partial o)) => Partial o -> CPU i s o ()
output_ = CPU . tell

{-# INLINE output #-}
output :: (Monoid (Partial o)) => Endo (Partial o) -> CPU i s o ()
output f = output_ $ appEndo f mempty

abort :: (Monoid (Partial o)) => CPU i s o a
abort = CPU $ throwE ()

runCPU
  :: (Generic o, Construct Identity o, FunctorB (HKD o), ProductBC (HKD o))
  => (s -> o) -> CPU i s o () -> (i -> State s o)
runCPU mkDef cpu inp = do
    s <- get
    let (s', writes) = execRWS (runExceptT $ unCPU cpu) (inp, s) s
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
