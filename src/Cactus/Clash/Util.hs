{-# LANGUAGE ScopedTypeVariables, DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Cactus.Clash.Util
    ( mealyState
    , mealyStateSlow
    , activeLow
    , activeHigh
    , countTo
    , countWhen
    , muxRR
    , nextIdx
    , prevIdx
    , succIdx
    , predIdx
    , debounce
    , diff
    , enable
    , extremum
    , regShiftIn
    , shiftInLeft
    , parity
    ) where

import Prelude ()
import Clash.Prelude
import Control.Monad.State
import Data.Word
import Data.Maybe (fromMaybe, isJust)
import qualified Data.List as L

mealyState :: (HiddenClockResetEnable dom, Undefined s)
           => (i -> State s o) -> s -> (Signal dom i -> Signal dom o)
mealyState = mealyStateSlow (pure True)

mealyStateSlow
    :: (HiddenClockResetEnable dom, Undefined s)
    => Signal dom Bool
    -> (i -> State s o)
    -> s
    -> (Signal dom i -> Signal dom o)
mealyStateSlow tick f s0 x = mealy step s0 (bundle (tick, x))
  where
    step s (tick, x) = let (y, s') = runState (f x) s
                       in (if tick then s' else s, y)

activeLow :: (Functor f) => f Bool -> f Bit
activeLow = fmap complement . activeHigh

activeHigh :: (Functor f) => f Bool -> f Bit
activeHigh = fmap boolToBit

countWhen
    :: forall a dom.
      (Undefined a, Num a, HiddenClockResetEnable dom)
    => Signal dom Bool -> Signal dom a
countWhen s = fix $ regEn 0 s . (1 +)

diff
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe a) -> Signal dom (Maybe a)
diff = mealy step False
  where
    step False new = (isJust new, new)
    step True new = (isJust new, Nothing)

countTo
    :: (Undefined a, Eq a, Num a, HiddenClockResetEnable dom)
    => Signal dom a -> Signal dom Bool
countTo n = cnt .==. n
  where
    cnt = register 0 $ mux (cnt .==. n) 0 (cnt + 1)

muxRR
    :: forall dom n a. (HiddenClockResetEnable dom, KnownNat n)
    => Signal dom Bool
    -> Vec n (Signal dom a)
    -> (Signal dom (Vec n Bool), Signal dom a)
muxRR next ss = let (mask, i) = unbundle $ moore step id (mask0, (0 :: Index n)) next
                in (mask, (!!) <$> bundle ss <*> i)
  where
    step s False = s
    step (mask, i) True = (rotateLeftS mask d1, nextIdx i)

    mask0 = repeat False <<+ True

nextIdx :: (Eq a, Enum a, Bounded a) => a -> a
nextIdx = fromMaybe minBound . succIdx

prevIdx :: (Eq a, Enum a, Bounded a) => a -> a
prevIdx = fromMaybe maxBound . predIdx

succIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
succIdx x | x == maxBound = Nothing
          | otherwise = Just $ succ x

predIdx :: (Eq a, Enum a, Bounded a) => a -> Maybe a
predIdx x | x == minBound = Nothing
          | otherwise = Just $ pred x

unsigned :: (KnownNat n) => SNat n -> Unsigned n -> Unsigned n
unsigned n = id

shiftInLeft :: (BitPack a, KnownNat (BitSize a)) => Bit -> a -> a
shiftInLeft b bs = unpack . pack . fst $ shiftInAt0 (unpack . pack $ bs) (b :> Nil)

debounce
    :: (HiddenClockResetEnable dom, KnownNat n, Eq a, Undefined a)
    => SNat n -> a -> Signal dom a -> Signal dom a
debounce n x0 = mealyState step (unsigned n 0, x0, x0)
  where
    step this = do
        (counter, last, prev) <- get
        let stable = counter == maxBound
            changing = this /= prev
            counter' = if changing then 0 else counter `boundedAdd` 1
            last' = if counter' == maxBound then this else last

        put (counter', last', this)
        return last'

enable :: Bool -> a -> Maybe a
enable False = const Nothing
enable True = Just

extremum :: (KnownNat n) => Vec n Bit -> Maybe Bit
extremum xs
  | xs == repeat low = Just low
  | xs == repeat high = Just high
  | otherwise = Nothing

regShiftIn
    :: (KnownNat n, Undefined a)
    => (HiddenClockResetEnable dom)
    => Vec n a -> Signal dom (Maybe a) -> (Signal dom (Vec n a), Signal dom (Maybe a))
regShiftIn = mealyB $ \xs x -> let out@(xs', _) = shiftIn x xs in (xs', out)
  where
    shiftIn :: (KnownNat n) => Maybe a -> Vec n a -> (Vec n a, Maybe a)
    shiftIn Nothing xs = (xs, Nothing)
    shiftIn (Just x) xs = let (xs', x :> Nil) = shiftInAt0 xs (singleton x)
                          in (xs', Just x)

parity :: (FiniteBits a) => a -> Bit
parity x = L.foldr xor low [ boolToBit $ testBit x i | i <- [0..n-1] ]
  where
    n = finiteBitSize x
