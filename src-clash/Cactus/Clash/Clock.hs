{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
module Cactus.Clash.Clock
    ( FromHz
    , ClockDivider
    , divider
    ) where

import Clash.Prelude
import Cactus.Clash.Util

type FromHz rate = 1_000_000_000_000 `Div` rate

type family ClockDivider conf (n :: Nat) where
    ClockDivider ('DomainConfiguration _ ps _ _ _ _) n = n `Div` ps

divider
    :: forall n proxy dom conf.
       (KnownNat n, KnownNat (ClockDivider conf n), HiddenClockResetEnable dom conf)
    => proxy n -> Signal dom Bool
divider _ = countTo (pure $ maxBound @(Index (ClockDivider conf n)))
