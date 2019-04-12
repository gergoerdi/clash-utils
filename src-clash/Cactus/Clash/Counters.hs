{-# LANGUAGE GADTs, DataKinds, PolyKinds, NoStarIsType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Cactus.Clash.Counters (Indices, counterProdSum) where

import Clash.Prelude
import Clash.Prelude.Moore
import Cactus.Clash.Util
import Cactus.Clash.Product
import Cactus.Clash.Sum
import Data.Word
import Data.Proxy
import Data.Kind
import Control.Monad.State

import Data.Maybe
import Control.Applicative

import Data.Singletons -- ((~>))
import Data.Singletons.Prelude.List (Map)

data SNats :: [Nat] -> Type where
    SNatsNil :: SNats '[]
    SNatsCons :: (KnownNat n) => SNats ns -> SNats (n : ns)

class KnownNats (ns :: [Nat]) where
    type Indices ns = (res :: [Type]) | res -> ns
    knownNats :: SNats ns

instance KnownNats '[] where
    type Indices '[] = '[]
    knownNats = SNatsNil

instance (KnownNat n, KnownNats ns) => KnownNats (n : ns) where
    type Indices (n : ns) = Index n : Indices ns
    knownNats = SNatsCons knownNats

sumSucc :: (KnownNats ns) => Sum (Indices ns) -> Maybe (Sum (Indices ns))
sumSucc = go knownNats
  where
    go :: forall ks. SNats ks -> Sum (Indices ks) -> Maybe (Sum (Indices ks))
    go SNatsNil _ = Nothing
    go (SNatsCons k) (Here x) = case succIdx x of
        Just x' -> Just $ Here x'
        Nothing -> There <$> next k
    go (SNatsCons k) (There y) = There <$> go k y

    next :: forall ks. SNats ks -> Maybe (Sum (Indices ks))
    next (SNatsCons _) = Just $ Here 0
    next _ = Nothing

sumMinBound :: (KnownNats (n : ns)) => proxy (n : ns) -> Sum (Indices (n : ns))
sumMinBound ns = go knownNats
  where
    go :: forall k ks. SNats (k : ks) -> Sum (Indices (k : ks))
    go (SNatsCons _) = Here 0

data CounterSum_ :: [Nat] ~> Type
type instance Apply CounterSum_ ns = Sum (Indices ns)

class IsCounterProd (nss :: [[Nat]]) where
    prodSucc :: proxy nss -> Product (Map CounterSum_ nss) -> Product (Map CounterSum_ nss)
    prodMinBound :: proxy nss -> Product (Map CounterSum_ nss)

instance IsCounterProd '[] where
    prodSucc _ PNil = PNil
    prodMinBound _ = PNil

instance (KnownNats (n:ns), IsCounterProd nss) => IsCounterProd ((n:ns) : nss) where
    prodSucc _ (xs :-: xss) = case sumSucc xs of
        Nothing -> sumMinBound (Proxy @(n:ns)) :-: prodSucc (Proxy @nss) xss
        Just xs' -> xs' :-: xss

    prodMinBound _ = sumMinBound (Proxy @(n:ns)) :-: prodMinBound (Proxy @nss)

counterProdSum
  :: ( HiddenClockReset dom gated synchronous
    , KnownLength (Map CounterSum_ nss)
    , IsCounterProd nss
    , Undefined (Product (Map CounterSum_ nss))
    )
  => proxy nss
  -> Signal dom Bool
  -> Product (ToSignals dom (Map CounterSum_ nss))
counterProdSum nss tick = unbundleProd r
  where
    r = regEn (prodMinBound nss) tick $ prodSucc nss <$> r
