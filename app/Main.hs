module Main where

import           NesEmu.Cpu

main :: IO ()
main = do
  print $ interpret newCpu [0xA9, 0x7F, 0x00]
