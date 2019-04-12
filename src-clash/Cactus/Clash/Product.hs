{-# LANGUAGE GADTs, DataKinds, PolyKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilyDependencies, FlexibleInstances #-}
{-# LANGUAGE NoStarIsType #-}
module Cactus.Clash.Product where

import Clash.Prelude
import Data.Kind

type family Product (ts :: [Type]) = (res :: Type) | res -> ts where
    Product '[] = ()
    Product (t:ts) = (t, Product ts)

pattern PNil = ()
pattern (:-:) x xs = (x, xs)
infixr 5 :-:

headP :: Product (t : ts) -> t
headP (x :-: xs) = x

tailP :: Product (t : ts) -> Product ts
tailP (x :-: xs) = xs

data SLength :: [k] -> Type where
    SLenNil :: SLength '[]
    SLenCons :: SLength xs -> SLength (x : xs)

class KnownLength (xs :: [k]) where
    knownLength :: SLength xs

instance KnownLength '[] where knownLength = SLenNil
instance (KnownLength xs) => KnownLength (x : xs) where knownLength = SLenCons knownLength

type family ToSignals (dom :: Domain) (ts :: [Type]) = (r :: [Type]) | r -> ts dom where
    ToSignals dom '[] = dom ::: '[]
    ToSignals dom (t : ts) = Signal dom t : ToSignals dom ts

unbundleProd :: (KnownLength ts) => Signal dom (Product ts) -> Product (ToSignals dom ts)
unbundleProd = go knownLength
  where
    go :: forall dom us. SLength us -> Signal dom (Product us) -> Product (ToSignals dom us)
    go SLenNil _ = PNil
    go (SLenCons n) xs = (headP <$> xs) :-: (go n $ tailP <$> xs)

bundleProd :: (KnownLength ts) => Product (ToSignals dom ts) -> Signal dom (Product ts)
bundleProd = go knownLength
  where
    go :: forall dom us. SLength us -> Product (ToSignals dom us) -> Signal dom (Product us)
    go SLenNil PNil = pure PNil
    go (SLenCons n) (x :-: xs) = (:-:) <$> x <*> go n xs
