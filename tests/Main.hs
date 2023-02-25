module Main where

import qualified CpuSpec    as Cpu
import           Test.Hspec

main :: IO ()
main = hspec $ do
  Cpu.spec
