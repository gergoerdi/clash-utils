{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Cactus.Clash.TH.BitPattern (bitPattern) where

import Clash.Prelude
import Language.Haskell.TH
import Data.List as L
import Data.Maybe (fromMaybe)

fromBits :: [Bit] -> Integer
fromBits = L.foldl (\v b -> v `shiftL` 1 .|. fromIntegral b) 0

bitPattern :: String -> Q Pat
bitPattern s = [p| (($mask .&.) -> $pattern) |]
  where
    bs = parse <$> s

    mask = litE . IntegerL . fromBits $ maybe 0 (const 1) <$> bs
    pattern = litP . IntegerL . fromBits $ fromMaybe 0 <$> bs

    parse '.' = Nothing
    parse '0' = Just 0
    parse '1' = Just 1
