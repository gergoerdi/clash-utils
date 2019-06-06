{-# LANGUAGE DeriveGeneric, UndecidableInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Cactus.Clash.CPU.Internal where

import Clash.Prelude hiding (lift)
import GHC.Generics
import Control.Monad.Identity
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Function (on)

type family HKD f a where
    HKD Identity a = a
    HKD f        a = f a

class GUpdate i u where
    gupdate :: i p -> u p -> i p

instance GUpdate (K1 a k) (K1 a (Last k)) where
    {-# INLINE gupdate #-}
    gupdate (K1 x) (K1 mx) = K1 $ fromMaybe x $ getLast mx

instance (GUpdate i u, GUpdate i' u') => GUpdate (i :*: i') (u :*: u') where
    {-# INLINE gupdate #-}
    gupdate (x :*: x') (mx :*: mx') = gupdate x mx :*: gupdate x' mx'

instance (GUpdate i u) => GUpdate (M1 a b i) (M1 a' b' u) where
    {-# INLINE gupdate #-}
    gupdate (M1 x) (M1 mx) = M1 $ gupdate x mx

class GMonoid i where
    gmempty :: i p
    gmappend :: i p -> i p -> i p

instance GMonoid U1 where
    {-# INLINE gmempty #-}
    {-# INLINE gmappend #-}

    gmempty = U1
    gmappend U1 U1 = U1

instance (Monoid k) => GMonoid (K1 a k) where
    {-# INLINE gmempty #-}
    {-# INLINE gmappend #-}

    gmempty = K1 mempty
    gmappend (K1 x) (K1 y) = K1 $ mappend x y

instance (GMonoid i, GMonoid i') => GMonoid (i :*: i') where
    {-# INLINE gmempty #-}
    {-# INLINE gmappend #-}

    gmempty = gmempty :*: gmempty
    gmappend (x :*: x') (y :*: y') = gmappend x y :*: gmappend x' y'

instance (GMonoid i) => GMonoid (M1 a b i) where
    {-# INLINE gmempty #-}
    {-# INLINE gmappend #-}

    gmempty = M1 gmempty
    gmappend (M1 x) (M1 y) = M1 $ gmappend x y

newtype G a = G{ unG :: a }

instance (Generic a, GMonoid (Rep a)) => Semigroup (G a) where
    {-# INLINE (<>) #-}
    x <> y = G . to $ (gmappend `on` (from . unG)) x y

instance (Generic a, GMonoid (Rep a)) => Monoid (G a) where
    {-# INLINE mempty #-}
    mempty = G . to $ gmempty

update :: (Generic i, Generic u, GUpdate (Rep i) (Rep u)) => i -> u -> i
update initial update = to $ gupdate (from initial) (from update)
