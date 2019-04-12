{-# LANGUAGE TypeFamilyDependencies, EmptyCase, PatternSynonyms, NoStarIsType #-}
module Cactus.Clash.Sum where

import Clash.Prelude
import Control.DeepSeq
import Data.Kind (Type)

newtype Sum_ a = Sum_ a

data Void

type family Sum (ts :: [Type]) = (res :: Type) | res -> ts where
    Sum '[] = Void
    Sum (t:ts) = Either t (Sum ts)

pattern Here :: t -> Either t (Sum ts)
pattern Here x = Left x

pattern There :: Sum ts -> Either t (Sum ts)
pattern There y = Right y

the :: Sum '[a] -> a
the (Here x) = x
the (There y) = case y of {}
