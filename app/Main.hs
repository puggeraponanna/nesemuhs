module Main where

import           NesEmu.Cpu
import           NesEmu.Cpu.Memory  (memoryRead)
import           NesEmu.Rom
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
            cpu' <- runDebug cpu
            putStrLn $ printf "nestest results: %02X %02X" (memoryRead cpu' 0x0002) (memoryRead cpu' 0x0003)
            return ()
