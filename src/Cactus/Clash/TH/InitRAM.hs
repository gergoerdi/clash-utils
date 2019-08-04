{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Cactus.Clash.TH.InitRAM where

import Clash.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List as L
import System.FilePath
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import System.Directory

writeRAMImage :: Int -> Int -> FilePath -> IO ()
writeRAMImage n width fn = do
    createDirectoryIfMissing True (takeDirectory fn)
    writeFile fn $ unlines bvs
  where
    bvs = L.replicate n $ L.replicate width '0'

createRAMImage :: Int -> Int -> ExpQ
createRAMImage n width = do
    Module pkg (ModName mod) <- thisModule
    i <- fromMaybe 0 <$> qGetQ
    qPutQ $ succ i
    let fn = "null-" <> mod <> "-" <> show (i :: Int) <> "-" <> show n <> "x" <> show width <.> "hex"
    liftIO $ writeRAMImage n width fn
    litE $ stringL fn

blockRam_
  :: Int -> Int -> ExpQ
blockRam_ n w =
    [e|
     let ram :: forall addr n value. (Enum addr, BitPack value, KnownNat (BitSize value))
             => forall dom. (HiddenClockResetEnable dom)
             => Signal dom addr
             -> Signal dom (Maybe (addr, value))
             -> Signal dom value
         ram r w = unpack <$> blockRamFile (SNat @($n')) $img r (fmap (fmap pack) <$> w)
     in ram
    |]
  where
    n' = litT . numTyLit . fromIntegral $ n
    img = createRAMImage n w
