{-# LANGUAGE DataKinds, PolyKinds #-}
module Cactus.Clash.Counters (counterPlus', counterMul) where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.Product
import Data.Word
import Data.Proxy
import Control.Monad.State

import Data.Maybe
import Control.Applicative

import Data.Singletons -- ((~>))
import Data.Singletons.Prelude.List (Map)

counter
    :: (HiddenClockReset dom gated synchronous, KnownNat n)
    => proxy n
    -> Signal dom Bool
    -> Signal dom Bool
    -> (Signal dom (Maybe (Index n)), Signal dom Bool)
counter _ en tick = unbundle $ mealyState step Nothing $ bundle (en, tick)
  where
    step (en, tick) = do
        when en $ modify $ \i ->
            maybe (0 <$ guard tick) succIdx i
        i' <- get
        return (i', i' == Just maxBound)

class IsCounterPlus ns where
    type CounterSum ns (dom :: Domain) :: [*]

    counterPlus'
        :: (HiddenClockReset dom gated synchronous)
        => proxy ns
        -> Bool
        -> Signal dom Bool
        -> Signal dom Bool
        -> (Product (CounterSum ns dom), Signal dom Bool)

instance IsCounterPlus '[] where
    type CounterSum '[] dom = dom ::: '[]

    counterPlus' _ _ en tick = (PNil, tick)

instance (KnownNat n, IsCounterPlus ns) => IsCounterPlus (n:ns) where
    type CounterSum (n:ns) dom = Signal dom (Maybe (Index n)) : CounterSum ns dom

    counterPlus' _ isFirst en tick = (this :-: that, end)
      where
        (this, next) = counter (Proxy @n) en (register isFirst tick)
        (that, end) = counterPlus' (Proxy @ns) False en next

counterPlus
    :: (HiddenClockReset dom gated synchronous, IsCounterPlus ns)
    => proxy ns
    -> (Product (CounterSum ns dom), Signal dom Bool)
counterPlus ns = (counters, end)
  where
    (counters, end) = counterPlus' ns True (pure True) end

class IsCounterProd (nss :: [[Nat]]) where
    type CounterProd nss (dom :: Domain) :: [[*]]

    counterMul'
        :: (HiddenClockReset dom gated synchronous)
        => proxy nss
        -> Signal dom Bool
        -> Product (Map Product_ (CounterProd nss dom))

instance IsCounterProd '[] where
    type CounterProd '[] dom = dom ::: '[]

    counterMul' _ _ = PNil

instance (IsCounterPlus ns, IsCounterProd nss) => IsCounterProd (ns : nss) where
    type CounterProd (ns : nss) dom = CounterSum ns dom : CounterProd nss dom

    counterMul' _ prev = this :-: them
      where
        (this, endThis) = counterPlus' (Proxy @ns) True prev endThis
        them = counterMul' (Proxy @nss) (register True endThis)

data Product_ :: [*] ~> *
type instance Apply Product_ ts = Product ts

counterMul
    :: (HiddenClockReset dom gated synchronous, IsCounterProd nss)
    => proxy nss
    -> Product (Map Product_ (CounterProd nss dom))
counterMul nss = counterMul' nss (pure True)
