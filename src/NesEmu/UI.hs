{-# LANGUAGE OverloadedStrings #-}

module NesEmu.UI where

import           Control.Monad          (unless)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector.Storable   as V
import           Data.Word
import           Foreign.C.Types        (CInt)
import           Foreign.Ptr            (castPtr)
import           NesEmu.Cpu
import           NesEmu.Cpu.Types
import           NesEmu.Ppu
import qualified SDL
import           SDL                    (($=))

screenWidth, screenHeight :: CInt
screenWidth = 256
screenHeight = 240

windowScale :: CInt
windowScale = 3

runUI :: Cpu -> PpuState -> IO ()
runUI initialCpu initialPpu = do
    SDL.initializeAll

    let windowConfig =
            SDL.defaultWindow
                { SDL.windowInitialSize = SDL.V2 (screenWidth * windowScale) (screenHeight * windowScale)
                }

    window <- SDL.createWindow "NesEmuHS" windowConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    texture <- SDL.createTexture renderer SDL.ARGB8888 SDL.TextureAccessStreaming (SDL.V2 screenWidth screenHeight)

    mainLoop renderer texture initialCpu initialPpu

    SDL.destroyTexture texture
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

mainLoop :: SDL.Renderer -> SDL.Texture -> Cpu -> PpuState -> IO ()
mainLoop renderer texture cpu ppu = do
    events <- SDL.pollEvents
    let quit = any isQuitEvent events

    (cpu', ppu') <- runFrame cpu ppu

    let frameBuffer = renderFrame ppu'
    V.unsafeWith frameBuffer $ \ptr -> do
        bs <- BSU.unsafePackCStringLen (castPtr ptr, fromIntegral $ V.length frameBuffer * 4)
        SDL.updateTexture texture Nothing bs (screenWidth * 4)

    SDL.clear renderer
    SDL.copy renderer texture Nothing Nothing
    SDL.present renderer

    unless quit $ mainLoop renderer texture cpu' ppu'

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _ SDL.QuitEvent) = True
isQuitEvent (SDL.Event _ (SDL.KeyboardEvent k)) =
    SDL.keyboardEventKeyMotion k == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym k) == SDL.KeycodeQ
isQuitEvent _ = False

runFrame :: Cpu -> PpuState -> IO (Cpu, PpuState)
runFrame cpu ppu = do
    let frameCycles = 29780
    let (cpu', ppu') = stepFrame frameCycles cpu ppu
    return (cpu', ppu')

stepFrame :: Int -> Cpu -> PpuState -> (Cpu, PpuState)
stepFrame 0 cpu ppu = (cpu, ppu)
stepFrame cycles cpu ppu =
    case step cpu of
        Nothing -> (cpu, ppu)
        Just (cpu', usedCycles) ->
            let ppu' = stepPpu (usedCycles * 3) ppu
             in if cycles <= usedCycles
                    then (cpu', ppu')
                    else stepFrame (cycles - usedCycles) cpu' ppu'
