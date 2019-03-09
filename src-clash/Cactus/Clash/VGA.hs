{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Cactus.Clash.VGA where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.Clock
import Cactus.Clash.Product
import Cactus.Clash.Counters
import Data.Proxy
import Data.Maybe

data Polarity
    = Positive
    | Negative
    deriving (Show)

class KnownPolarity (polarity :: Polarity) where
    toBit :: proxy polarity -> Bool -> Bit

instance KnownPolarity Positive where
    toBit _ = boolToBit

instance KnownPolarity Negative where
    toBit _ = negate . boolToBit

data VGATiming = VGATiming
    { polarity :: Polarity
    , visibleSize, pre, syncPulse, post :: Nat
    }
    deriving (Show)

data VGATimings = VGATimings
    { pixelClock :: Nat
    , vgaHorizTiming :: VGATiming
    , vgaVertTiming :: VGATiming
    }
    deriving (Show)

data VGADriver dom w h = VGADriver
    { vgaVSync :: Signal dom Bit
    , vgaHSync :: Signal dom Bit
    , vgaEndFrame :: Signal dom Bool
    , vgaEndLine :: Signal dom Bool
    , vgaX :: Signal dom (Maybe (Index w))
    , vgaY :: Signal dom (Maybe (Index h))
    }

data SVGATimings (timings :: VGATimings) where
    SVGATimings
      :: ( KnownNat w, KnownNat preH, KnownNat pulseH, KnownNat postH
        , KnownNat h, KnownNat preV, KnownNat pulseV, KnownNat postV
        , KnownPolarity polH, KnownPolarity polV
        )
      => SVGATimings ('VGATimings ps ('VGATiming polH w preH pulseH postH) ('VGATiming polV h preV pulseV postV))

vgaDriver
    :: forall timings dom gated synchronous s ps polH w preH pulseH postH polV h preV pulseV postV.
      (HiddenClockReset dom gated synchronous)
    => (dom ~ Dom s ps)
    => (timings ~ 'VGATimings ps ('VGATiming polH w preH pulseH postH) ('VGATiming polV h preV pulseV postV))
    => SVGATimings timings
    -> VGADriver dom w h
vgaDriver timings@SVGATimings = VGADriver{..}
  where
    horiz ::> vert ::> NilP = counterMul $ Proxy
        @'[ [w, preH, pulseH, postH]
          , [h, preV, pulseV, postV]
          ]

    vgaX ::> _ ::> (toSync (Proxy @polH) -> vgaHSync) ::> _ = horiz
    vgaY ::> _ ::> (toSync (Proxy @polV) -> vgaVSync) ::> _ = vert

    vgaEndLine = isFalling False (isJust <$> vgaX)
    vgaEndFrame = isFalling False (isJust <$> vgaY)

toSync
    :: (KnownPolarity polarity, HiddenClockReset dom gated synchronous)
    => proxy polarity -> Signal dom (Maybe a) -> Signal dom Bit
toSync polarity = fmap (toBit polarity) . register False . fmap isJust

-- | VGA 640*480@60Hz, 25.175 MHz pixel clock
vga640x480at60 :: SVGATimings ('VGATimings
    (FromHz 25_175_000)
    ('VGATiming Negative 640 16 96 48)
    ('VGATiming Negative 480 11  2 31))
vga640x480at60 = SVGATimings

-- | VGA 800x600@72Hz, 50 MHz pixel clock
vga800x600at72 :: SVGATimings ('VGATimings
    (FromHz 50_000_000)
    ('VGATiming Positive 800 56 120 64)
    ('VGATiming Positive 600 37   6 23))
vga800x600at72 = SVGATimings

-- | VGA 800x600@60Hz, 40 MHz pixel clock
vga800x600at60 :: SVGATimings ('VGATimings
    (FromHz 40_000_000)
    ('VGATiming Positive 800 40 128 88)
    ('VGATiming Positive 600  1   4 23))
vga800x600at60 = SVGATimings

-- | VGA 1024*768@60Hz, 65 MHz pixel clock
-- | VGA 800x600@60Hz, 40 MHz pixel clock
vga1024x768at60 :: SVGATimings ('VGATimings
    (FromHz 40_000_000)
    ('VGATiming Negative 1024 24 136 160)
    ('VGATiming Negative  768  3   6  29))
vga1024x768at60 = SVGATimings
