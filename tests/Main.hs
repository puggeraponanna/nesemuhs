module Main where

import qualified NesEmuTest.Cpu.Flags as Flags
import qualified NesEmuTest.Cpu.Memory as Memory
import Test.Hspec ( hspec )

main :: IO ()
main = hspec $ do
  Flags.spec
  Memory.spec
