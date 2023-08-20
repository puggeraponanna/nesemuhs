module NesEmuTest.Cpu.Memory where

import           NesEmu.Cpu
import           NesEmu.Cpu.Memory
import           NesEmu.Cpu.Types
import           Test.Hspec

spec :: Spec
spec = do
  describe "memoryRead" $ do
    it "reads a byte from memory" $ do
      let cpu = newCpu {memory = [0xAB, 0xCD]}
      memoryRead cpu 0 `shouldBe` 0xAB
      memoryRead cpu 1 `shouldBe` 0xCD

  describe "memoryWrite" $ do
    it "writes a byte to memory" $ do
      let cpu = newCpu {memory = [0x00, 0x00]}
      let cpu' = memoryWrite cpu 0 0xAB
      memory cpu' `shouldBe` [0xAB, 0x00]

  describe "ZeroPage addressing" $ do
    it "reads from zeropage address" $ do
      let cpu = loadProgram newCpu [0xA5, 0x10]
      let cpu' = memoryWrite cpu 0x10 0xAB
      registerA cpu' `shouldBe` 0xAB

  describe "Absolute addressing" $ do
    it "reads from absolute address" $ do
      let cpu = loadProgram newCpu [0xAD, 0x34, 0x12]
      let cpu' = memoryWrite cpu 0x1234 0xCD
      registerA cpu `shouldBe` 0xCD

  describe "IndirectX addressing" $ do
    it "reads from dereferenced address offset by X" $ do
      let cpu = loadProgram newCpu [0xA2, 0x05, 0xA1, 0x10]
      let cpu' = memoryWrite cpu 0x15 0x34
      let cpu' = memoryWrite cpu 0x1234 0xAB
      registerA cpu `shouldBe` 0xAB
