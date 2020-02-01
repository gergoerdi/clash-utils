{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Cactus.Clash.CPU
       ( CPU
       , input, inputs, output, abort
       , getStart, getsStart
       , runCPU
       , runCPUDebug
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

newtype CPU i s o a = CPU{ unCPU :: ExceptT () (RWS (i, s) () (s, o)) a }
    deriving newtype (Functor, Applicative, Monad)

instance MonadState s (CPU i s o) where
    get = CPU $ gets fst
    put s' = CPU $ modify $ \(_, o) -> (s', o)

instance (Construct Identity a, HasField' field (HKD a Identity) (Identity b)) => IsLabel field (b -> Endo a) where
    {-# INLINE fromLabel #-}
    fromLabel x = Endo $ runIdentity . construct . (field @field .~ pure x) . deconstruct @Identity

input :: CPU i s o i
input = inputs id

inputs :: (i -> a) -> CPU i s o a
inputs f = CPU . asks $ f . fst

getStart :: CPU i s o s
getStart = getsStart id

getsStart :: (s -> a) -> CPU i s o a
getsStart f = CPU . asks $ f . snd

-- {-# INLINE output_ #-}
-- output_ :: Partial o -> CPU i s o ()
-- output_ = undefined -- CPU . tell

{-# INLINE output #-}
output :: Endo o -> CPU i s o ()
output f = CPU $ modify $ \(s, o) -> (s, appEndo f o)

abort :: CPU i s o a
abort = CPU $ throwE ()

runCPU :: (s -> o) -> CPU i s o () -> (i -> State s o)
runCPU mkDef cpu inp = do
    s <- get
    let o0 = mkDef s
    let ((s', o), ()) = execRWS (runExceptT $ unCPU cpu) (inp, s) (s, o0)
    put s'
    return o

-- runCPU
--   :: (Generic o, Construct Identity o, FunctorB (HKD o), ProductBC (HKD o))
--   => (s -> o) -> CPU i s o () -> (i -> State s o)
-- runCPU mkDef cpu inp = do
--     s <- get
--     let (s', writes) = execRWS (runExceptT $ unCPU cpu) (inp, s) s
--     put s'
--     def <- gets mkDef
--     return $ update def writes

runCPUDebug :: (s -> o) -> CPU i s o () -> (i -> State s (s, o))
runCPUDebug mkDef cpu inp = do
    s0 <- get
    out <- runCPU mkDef cpu inp
    return (s0, out)

-- runCPUDebug
--   :: (Generic o, Construct Identity o, FunctorB (HKD o), ProductBC (HKD o))
--   => (s -> o) -> CPU i s o () -> (i -> State s (s, o))
-- runCPUDebug mkDef cpu inp = do
--     s0 <- get
--     out <- runCPU mkDef cpu inp
--     return (s0, out)

-- update :: (Generic a, Construct Identity a, FunctorB (HKD a), ProductBC (HKD a)) => a -> HKD a Last -> a
-- update initial edits = runIdentity . construct $ bzipWith (\i -> maybe i Identity . getLast) (deconstruct initial) edits
