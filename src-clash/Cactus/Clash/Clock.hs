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

type family ClockDivider dom (n :: Nat) where
    ClockDivider (Dom s ps) n = n `Div` ps

divider
    :: forall n proxy domain gated synchronous.
       (KnownNat n, KnownNat (ClockDivider domain n), HiddenClockReset domain gated synchronous)
    => proxy n -> Signal domain Bool
divider _ = countTo (pure $ maxBound @(Index (ClockDivider domain n)))
