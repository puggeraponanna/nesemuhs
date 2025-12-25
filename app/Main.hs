module Main where

import           NesEmu.Cpu
import           NesEmu.Cpu.Memory  (memoryRead)
import           NesEmu.Ppu
import           NesEmu.Rom
import           NesEmu.UI
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           Text.Printf        (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [romPath] -> runRom romPath
        _ -> do
            putStrLn "Usage: nesemu <rom_path>"
            exitFailure

runRom :: FilePath -> IO ()
runRom path = do
    result <- loadRom path
    case result of
        Left err -> do
            putStrLn $ "Error loading ROM: " ++ err
            exitFailure
        Right rom -> do
            let cpu = loadRomIntoCpu newCpu rom (Just 0xC000)
            runUI cpu newPpu
            return ()
