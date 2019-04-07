{-# LANGUAGE GADTs, StandaloneDeriving, DataKinds, PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies, FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}
module Cactus.Clash.Sum where

import Clash.Prelude
import Control.DeepSeq

data Sum (ts :: [*]) where
    Here :: a -> Sum (a : ts)
    There :: Sum ts -> Sum (a : ts)

instance Show (Sum '[]) where show x = case x of {}
deriving instance (Show t, Show (Sum ts)) => Show (Sum (t : ts))

instance Eq (Sum '[]) where _ == _ = True
deriving instance (Eq t, Eq (Sum ts)) => Eq (Sum (t : ts))

instance Ord (Sum '[]) where compare _ _ = EQ
deriving instance (Ord t, Ord (Sum ts)) => Ord (Sum (t : ts))

instance NFData (Sum '[]) where
    rnf x = case x of {}

instance (NFData t, NFData (Sum ts)) => NFData (Sum (t:ts)) where
    rnf (Here x) = rnf x
    rnf (There y) = rnf y

instance Bundle (Sum ts) where
    type Unbundled dom (Sum ts) = Signal dom (Sum ts)

instance Undefined (Sum ts) where
    deepErrorX = errorX
