{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
module Cactus.Clash.Clock
    ( FromHz
    , fromHz
    , ClockDivider
    , divider
    ) where

import Clash.Prelude
import Cactus.Clash.Util
import GHC.Natural

type FromHz rate = 1_000_000_000_000 `Div` rate

fromHz :: Integer -> Natural
fromHz hz = fromIntegral $ 1_000_000_000_000 `div` hz

type family ClockDivider conf (n :: Nat) where
    ClockDivider ('DomainConfiguration _ ps _ _ _ _) n = n `Div` ps

divider
    :: forall n proxy dom conf.
       (KnownNat n, KnownNat (ClockDivider conf n), HiddenClockResetEnable dom conf)
    => proxy n -> Signal dom Bool
divider _ = countTo (pure $ maxBound @(Index (ClockDivider conf n)))
