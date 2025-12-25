{-# LANGUAGE TemplateHaskell #-}

module NesEmu.Ppu where

import           Control.Lens
import qualified Data.Vector.Storable as V
import           Data.Word

data PpuState = PpuState
    { _vram     :: V.Vector Word8
    , _oam      :: V.Vector Word8
    , _scanline :: Int
    , _cycle    :: Int
    , _frame    :: Int
    }
    deriving (Show)

makeLenses ''PpuState

newPpu :: PpuState
newPpu =
    PpuState
        { _vram = V.replicate 0x4000 0
        , _oam = V.replicate 0x0100 0
        , _scanline = 0
        , _cycle = 0
        , _frame = 0
        }

renderFrame :: PpuState -> V.Vector Word32
renderFrame ppu = V.generate (256 * 240) $ \i ->
    let x = i `mod` 256
        y = i `div` 256
        f = _frame ppu
     in fromIntegral $ (x + f) `mod` 256 + ((y + f) `mod` 256) * 256 + ((x * y) `mod` 256) * 65536

stepPpu :: Int -> PpuState -> PpuState
stepPpu cycles ppu =
    let newCycle = (_cycle ppu + cycles)
        scanlinesAdded = newCycle `div` 341
        finalCycle = newCycle `mod` 341
        newScanline = (_scanline ppu + scanlinesAdded)
        framesAdded = newScanline `div` 262
        finalScanline = newScanline `mod` 262
        finalFrame = (_frame ppu + framesAdded)
     in ppu{_cycle = finalCycle, _scanline = finalScanline, _frame = finalFrame}
