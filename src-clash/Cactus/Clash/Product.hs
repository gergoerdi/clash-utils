{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
{-# LANGUAGE NoStarIsType #-}
module Cactus.Clash.Product where

import Clash.Prelude
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Kind

class IsProduct (ts :: [Type]) where
    data Product ts
    type UnbundledP (dom :: Domain) ts = (res :: [Type]) | res -> dom ts

    bundleP :: Product (UnbundledP dom ts) -> Signal dom (Product ts)
    unbundleP :: Signal dom (Product ts) -> Product (UnbundledP dom ts)

instance IsProduct '[] where
    data Product '[] = NilP
        deriving (Generic, NFData, Show, Undefined)
    type UnbundledP dom '[] = dom ::: '[]

    bundleP NilP = pure NilP
    unbundleP _ = NilP

infixr 5 ::>

instance (IsProduct ts) => IsProduct (t : ts) where
    data Product (t : ts) = t ::> (Product ts)

    type UnbundledP dom (t:ts) = Signal dom t : UnbundledP dom ts

    bundleP (x ::> xs) = (::>) <$> x <*> bundleP xs
    unbundleP s = (headP <$> s) ::> (unbundleP (tailP <$> s))

headP :: Product (t : ts) -> t
headP (x ::> xs) = x

tailP :: Product (t : ts) -> Product ts
tailP (x ::> xs) = xs

deriving instance (Show t, Show (Product ts)) => Show (Product (t : ts))
deriving instance (Generic t, Generic (Product ts)) => Generic (Product (t : ts))
deriving instance (Generic t, Generic (Product ts), NFData t, NFData (Product ts)) => NFData (Product (t : ts))
deriving instance (Generic t, Generic (Product ts), Undefined t, Undefined (Product ts)) => Undefined (Product (t : ts))

instance (IsProduct ts) => Bundle (Product ts) where
    type Unbundled dom (Product ts) = Product (UnbundledP dom ts)

    bundle = bundleP
    unbundle = unbundleP
