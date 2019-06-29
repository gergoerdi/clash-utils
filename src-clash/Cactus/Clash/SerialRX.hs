{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, TupleSections #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE NoStarIsType #-}
module Cactus.Clash.SerialRX
    ( RXState(..)
    , rx
    ) where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.Clock

import GHC.Generics (Generic, Generic1)
import Control.DeepSeq

import Control.Category ((>>>))
import Control.Monad.State hiding (state)
import Control.Monad.Trans.Writer
import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Monoid
import Data.Proxy

data RXState = RXState
    { buf1, buf2 :: Bit
    , cnt :: Integer
    , byte :: Word8
    , state :: MicroState
    }
    deriving (Generic, NFData, Show, Undefined)

data MicroState
    = RXIdle
    | RXStart
    | RXBit (Index 8)
    | RXStop
    | RXCleanup
    deriving (Generic, NFData, Show, Undefined)

rx0 :: Integer -> Bit -> State RXState (Maybe Word8)
rx0 divider bit = do
    s@RXState{..} <- get
    modify $ \s -> s{ buf2 = buf1, buf1 = bit, cnt = cnt - 1 }

    fmap getLast $ execWriterT $ case state of
        RXIdle -> do
            when (buf2 == low) $ goto RXStart
        RXStart -> when (cnt == divider `div` 2) $ do
            goto $ if buf2 == low then RXBit 0 else RXIdle
        RXBit i -> when (cnt == 0) $ do
            modify $ \s -> s{ byte = shiftInLeft buf2 byte }
            goto $ maybe RXStop RXBit $ succIdx i
        RXStop -> when (cnt == 0) $ do
            tell $ Last . Just $ byte
            goto RXCleanup
        RXCleanup -> goto RXIdle
  where
    goto st = modify $ \s -> s{ cnt = divider, state = st }

rx
    :: forall rate dom proxy. (KnownNat rate, KnownNat (ClockDivider dom (2 * rate)))
    => (HiddenClockResetEnable dom)
    => proxy rate
    -> Signal dom Bit
    -> Signal dom (Maybe Word8)
rx rate = mealyState (rx0 $ natVal (Proxy @(ClockDivider dom (2 * rate)))) s0
  where
    s0 = RXState
        { buf1 = 0
        , buf2 = 0
        , cnt = 0
        , byte = 0
        , state = RXIdle
        }
