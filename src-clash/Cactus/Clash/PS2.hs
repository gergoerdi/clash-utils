{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Cactus.Clash.PS2
    ( PS2(..)
    , samplePS2
    , decodePS2
    , KeyEvent(..)
    , ScanCode(..)
    , parseScanCode
    ) where

import Clash.Prelude
import Cactus.Clash.Util
import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.Monoid
import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

data PS2 dom = PS2
    { ps2Clk :: Signal dom Bit
    , ps2Data :: Signal dom Bit
    }

samplePS2
    :: (HiddenClockResetEnable dom)
    => PS2 dom -> Signal dom (Maybe Bit)
samplePS2 PS2{..} = enable <$> isFalling low ps2Clk' <*> ps2Data'
  where
    ps2Clk' = debounce d3 low ps2Clk
    ps2Data' = debounce d3 low ps2Data

data PS2State
    = Idle
    | Bit (Unsigned 8) (Index 8)
    | Parity (Unsigned 8)
    | Stop (Maybe (Unsigned 8))
    deriving (Show, Eq, Generic, Undefined)

decodePS2
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe Bit) -> Signal dom (Maybe (Unsigned 8))
decodePS2 = flip mealyState Idle $ \bit -> fmap getLast . execWriterT . forM_ bit $ \bit -> do
    state <- get
    case state of
        Idle -> do
            when (bit == low) $ put $ Bit 0 0
        Bit x i -> do
            let x' = shiftInLeft bit x
            put $ maybe (Parity x') (Bit x') $ succIdx i
        Parity x -> do
            let checked = bit /= parity x
            put $ Stop $ enable checked x
        Stop x -> do
            when (bit == high) $ tell $ Last x
            put Idle

data KeyEvent = KeyPress | KeyRelease
    deriving (Generic, NFData, Eq, Show, Undefined)

data ScanCode = ScanCode KeyEvent (Unsigned 9)
    deriving (Generic, NFData, Eq, Show, Undefined)

data ScanState
    = Init
    | Extended
    | Code KeyEvent Bit
    deriving (Show, Generic, Undefined)

-- TODO: rewrite this for clarity.
-- All it does is it parses 0xE0 0xXX into an extended code, and
-- everything else into an 8-bit code. The only complication is that
-- the key release marker 0xF0 is always the second-to-last byte. Who
-- does that?!?
parseScanCode
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8)) -> Signal dom (Maybe ScanCode)
parseScanCode = flip mealyState Init $ \raw -> fmap getLast . execWriterT . forM_ raw $ \raw -> do
    let finish ev ext = do
            tell $ Last . Just $ ScanCode ev $ bitCoerce (ext, raw)
            put Init
    state <- get
    case state of
        Init | raw == 0xe0 -> put $ Extended
             | raw == 0xf0 -> put $ Code KeyRelease low
             | otherwise -> finish KeyPress low
        Extended | raw == 0xf0 -> put $ Code KeyRelease high
                 | otherwise -> finish KeyPress high
        Code ev ext -> finish ev ext
