{-# LANGUAGE ScopedTypeVariables, DataKinds, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Cactus.Clash.Util
    ( mealyState
    , mealyStateSlow
    , activeLowReset
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

mealyState :: (HiddenClockReset domain gated synchronous, Undefined s)
           => (i -> State s o) -> s -> (Signal domain i -> Signal domain o)
mealyState = mealyStateSlow (pure True)

mealyStateSlow
    :: (HiddenClockReset domain gated synchronous, Undefined s)
    => Signal domain Bool
    -> (i -> State s o)
    -> s
    -> (Signal domain i -> Signal domain o)
mealyStateSlow tick f s0 x = mealy step s0 (bundle (tick, x))
  where
    step s (tick, x) = let (y, s') = runState (f x) s
                       in (if tick then s' else s, y)

activeLowReset :: Reset domain Asynchronous -> Reset domain Asynchronous
activeLowReset = unsafeToAsyncReset . (not <$>) . unsafeFromAsyncReset

activeLow :: (Functor f) => f Bool -> f Bit
activeLow = fmap complement . activeHigh

activeHigh :: (Functor f) => f Bool -> f Bit
activeHigh = fmap boolToBit

countWhen
    :: forall a domain gated synchronous.
      (Undefined a, Num a, HiddenClockReset domain gated synchronous)
    => Signal domain Bool -> Signal domain a
countWhen s = fix $ regEn 0 s . (1 +)

diff
    :: (HiddenClockReset domain gated synchronous)
    => Signal domain (Maybe a) -> Signal domain (Maybe a)
diff = mealy step False
  where
    step False new = (isJust new, new)
    step True new = (isJust new, Nothing)

countTo
    :: (Undefined a, Eq a, Num a, HiddenClockReset domain gated synchronous)
    => Signal domain a -> Signal domain Bool
countTo n = cnt .==. n
  where
    cnt = register 0 $ mux (cnt .==. n) 0 (cnt + 1)

muxRR
    :: forall domain gated synchronous n a. (HiddenClockReset domain gated synchronous, KnownNat n)
    => Signal domain Bool
    -> Vec n (Signal domain a)
    -> (Signal domain (Vec n Bool), Signal domain a)
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
    :: (HiddenClockReset domain gated synchronous, KnownNat n, Eq a, Undefined a)
    => SNat n -> a -> Signal domain a -> Signal domain a
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
    :: (HiddenClockReset dom gated synchronous, KnownNat n, Undefined a)
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
