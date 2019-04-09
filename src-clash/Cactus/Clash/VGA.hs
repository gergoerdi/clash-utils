{-# LANGUAGE RecordWildCards, ViewPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs, DataKinds, NoStarIsType #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Cactus.Clash.VGA where

import Clash.Prelude
import Cactus.Clash.Util
import Cactus.Clash.Clock
import Cactus.Clash.Product
import Cactus.Clash.Sum
import Cactus.Clash.Counters
import Data.Proxy
import Data.Maybe

data Polarity
    = Positive
    | Negative
    deriving (Show)

data VGATiming (pre :: Nat) (pulse :: Nat) (post :: Nat) = VGATiming Polarity

data VGATimings (pixelClock :: Nat) (w :: Nat) (h :: Nat) where
    VGATimings
        :: ( KnownNat fps, KnownNat w, KnownNat h
          , KnownNat preH, KnownNat pulseH, KnownNat postH
          , KnownNat preV, KnownNat pulseV, KnownNat postV)
        => VGATiming preH pulseH postH
        -> VGATiming preV pulseV postV
        -> VGATimings (FromHz (fps * (w + preH + pulseH + postH) * (h + preV + pulseV + postV))) w h

data VGADriver dom w h = VGADriver
    { vgaVSync :: Signal dom Bit
    , vgaHSync :: Signal dom Bit
    , vgaEndFrame :: Signal dom Bool
    , vgaEndLine :: Signal dom Bool
    , vgaX :: Signal dom (Maybe (Index w))
    , vgaY :: Signal dom (Maybe (Index h))
    }

vgaCounters
    :: proxy1 w -> VGATiming preH pulseH postH
    -> proxy2 h -> VGATiming preV pulseV postV
    -> Proxy '[ [w, preH, pulseH, postH]
             , [h, preV, pulseV, postV]
             ]
vgaCounters _ _ _ _ = Proxy

toSync :: (HiddenClockReset dom gated synchronous) => Polarity -> Signal dom (Maybe a) -> Signal dom Bit
toSync polarity = fmap toBit . register False . fmap isJust
  where
    toBit = case polarity of
        Positive -> boolToBit
        Negative -> negate . boolToBit

vgaDriver
    :: forall timings dom gated synchronous s ps polH w preH pulseH postH polV h preV pulseV postV.
      (HiddenClockReset dom gated synchronous)
    => (dom ~ Dom s ps)
    => VGATimings ps w h
    -> VGADriver dom w h
vgaDriver timings@(VGATimings horizTime@(VGATiming polarityH) vertTime@(VGATiming polarityV)) = VGADriver{..}
  where
    horiz :-: vert :-: PNil = counterProdSum (vgaCounters (Proxy @w) horizTime (Proxy @h) vertTime) (pure True)

    unpackVGA :: Sum (Indices '[visible, pre, sync, post]) -> (Maybe (Index visible), Maybe (Index sync))
    unpackVGA (Here x) = (Just x, Nothing)
    unpackVGA (There (There (Here cnt))) = (Nothing, Just cnt)
    unpackVGA _ = (Nothing, Nothing)

    (vgaX, toSync polarityH -> vgaHSync) = unbundle $ unpackVGA <$> horiz
    (vgaY, toSync polarityV -> vgaVSync) = unbundle $ unpackVGA <$> vert

    vgaEndLine = isFalling False (isJust <$> vgaX)
    vgaEndFrame = isFalling False (isJust <$> vgaY)

-- | VGA 640*480@60Hz, 25.152 MHz pixel clock
vga640x480at60 :: VGATimings _ 640 480
vga640x480at60 = VGATimings @60
    (VGATiming @16 @96 @48 Negative)
    (VGATiming @11 @2  @31 Negative)

-- | VGA 800x600@72Hz, 50 MHz pixel clock
vga800x600at72 :: VGATimings _ 800 600
vga800x600at72 = VGATimings @72
    (VGATiming @56 @120 @64 Positive)
    (VGATiming @37 @6   @23 Positive)

-- | VGA 800x600@60Hz, 40 MHz pixel clock
vga800x600at60 :: VGATimings _ 800 600
vga800x600at60 = VGATimings @60
    (VGATiming @40 @128 @88 Positive)
    (VGATiming @1  @4   @23 Positive)

-- | VGA 1024x768@60Hz, 65 MHz pixel clock
vga1024x768at60 :: VGATimings _ 1024 768
vga1024x768at60 = VGATimings @60
    (VGATiming @24 @136 @160 Negative)
    (VGATiming @3  @6   @29  Negative)
