{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleContexts #-}
module Cactus.Clash.SerialTX
    ( TXOut(..)
    , txDyn
    , tx
    , fifo
    ) where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.Clock

import Control.Category ((>>>))
import Control.Monad.State
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Foldable (for_)

data TXState n
    = TXIdle
    | TXStart (Unsigned n)
    | TXBit (Unsigned n) (Index n)
    deriving (Show, Eq, Generic, Undefined)

data TXOut dom = TXOut{ txReady :: Signal dom Bool, txOut :: Signal dom Bit }

rotateRightS' :: forall a d. (BitPack a, KnownNat (BitSize a)) => SNat d -> a -> a
rotateRightS' d x = unpack . pack $ rotateRightS (unpack . pack $ x :: Vec (BitSize a) Bit) d

tx0 :: (KnownNat n) => Maybe (Unsigned n) -> State (TXState n) (Bool, Bit)
tx0 input = do
    s <- get
    case s of
        TXIdle -> do
            for_ input $ put . TXStart
            return (True, high)
        TXStart x -> do
            put $ TXBit x 0
            return (False, low)
        TXBit x i -> do
            let x' = rotateRightS' d1 x
            put $ maybe TXIdle (TXBit x') $ succIdx i
            return (False, lsb . pack $ x)

txDyn
    :: (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (Unsigned n))
    -> TXOut dom
txDyn tick inp = TXOut{..}
  where
    (txReady, txOut) = unbundle $ mealyStateSlow tick tx0 TXIdle inp

tx
    :: (KnownNat n, HiddenClockResetEnable dom)
    => (KnownNat rate, KnownNat (ClockDivider dom rate))
    => proxy rate
    -> Signal dom (Maybe (Unsigned n))
    -> TXOut dom
tx rate = txDyn (divider rate)

fifo
    :: forall a. (Undefined a)
    => forall dom conf. (HiddenClockResetEnable dom)
    => Signal dom (Maybe a) -> Signal dom Bool -> (Signal dom (Maybe a), Signal dom Bool)
fifo input consumed = unbundle $ mealyState step Nothing $ bundle (input, consumed)
  where
    step (input, consumed) = do
        if consumed then case input of
            Nothing -> return (Nothing, True)
            Just x -> do
                put $ Just x
                return (Just x, False)
          else do
            x <- get
            return (x, isNothing x)
