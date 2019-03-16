{-# LANGUAGE GADTs, StandaloneDeriving, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies, FlexibleInstances #-}
{-# LANGUAGE NoStarIsType #-}
module Cactus.Clash.Product where

import Clash.Prelude
import Control.DeepSeq
import Data.Kind

data Product (ts :: [Type]) where
    PNil :: Product '[]
    (:-:) :: a -> Product ts -> Product (a : ts)
infixr 5 :-:

instance Show (Product '[]) where show PNil = "PNil"
deriving instance (Show t, Show (Product ts)) => Show (Product (t : ts))

instance Eq (Product '[]) where _ == _ = True
deriving instance (Eq t, Eq (Product ts)) => Eq (Product (t : ts))

instance Ord (Product '[]) where compare _ _ = EQ
deriving instance (Ord t, Ord (Product ts)) => Ord (Product (t : ts))

instance NFData (Product '[]) where
    rnf PNil = ()

instance (NFData t, NFData (Product ts)) => NFData (Product (t:ts)) where
    rnf (x :-: xs) = rnf x `seq` rnf xs

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

instance (KnownLength ts) => Bundle (Product ts) where
    type Unbundled dom (Product ts) = Product (ToSignals dom ts)

    bundle = go knownLength
      where
        go :: forall dom us. SLength us -> Product (ToSignals dom us) -> Signal dom (Product us)
        go SLenNil PNil = pure PNil
        go (SLenCons n) (x :-: xs) = (:-:) <$> x <*> go n xs

    unbundle = go knownLength
      where
        go :: forall dom us. SLength us -> Signal dom (Product us) -> Product (ToSignals dom us)
        go SLenNil _ = PNil
        go (SLenCons n) xs = (headP <$> xs) :-: (go n $ tailP <$> xs)
