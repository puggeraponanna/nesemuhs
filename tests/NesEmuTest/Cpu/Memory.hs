module NesEmuTest.Cpu.Memory where

import           NesEmu.Cpu
import           NesEmu.Cpu.Memory
import           NesEmu.Cpu.Opcodes
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

  describe "memoryWrite16" $ do
    it "writes a 16-bit value across two bytes" $ do
      let cpu = newCpu {memory = replicate 0xFFFF 0}
      let cpu' = memoryWrite16 cpu 0x1234 0xABCD
      memoryRead cpu' 0x1234 `shouldBe` 0xCD
      memoryRead cpu' 0x1235 `shouldBe` 0xAB

  describe "memoryRead16" $ do
    it "reads a 16-bit value from two bytes" $ do
      let cpu = newCpu {memory = [0xCD, 0xAB]}
      memoryRead16 cpu 0 `shouldBe` 0xABCD

  describe "getOperandAddress" $ do
    describe "Immediate" $ do
      it "returns the program counter" $ do
        let cpu = newCpu {programCounter = 0x1000}
        getOperandAddress cpu Immediate `shouldBe` 0x1000

    describe "ZeroPage" $ do
      it "returns the address from zeropage" $ do
        let cpu = newCpu {memory = [0x34, 0x12]}
        getOperandAddress cpu ZeroPage `shouldBe` 0x34

    describe "ZeroPageX" $ do
      it "returns zeropage + X register" $ do
        let cpu = newCpu {registerX = 0x05, memory = [0x30]}
        getOperandAddress cpu ZeroPageX `shouldBe` 0x35

    describe "ZeroPageY" $ do
      it "returns zeropage + Y register" $ do
        let cpu = newCpu {registerY = 0x05, memory = [0x30]}
        getOperandAddress cpu ZeroPageY `shouldBe` 0x35

    describe "Absolute" $ do
      it "returns the 16-bit address" $ do
        let cpu = newCpu {memory = [0x34, 0x12]}
        getOperandAddress cpu Absolute `shouldBe` 0x1234

    describe "AbsoluteX" $ do
      it "returns absolute address + X register" $ do
        let cpu = newCpu {registerX = 0x05, memory = [0x34, 0x12]}
        getOperandAddress cpu AbsoluteX `shouldBe` 0x1239

    describe "AbsoluteY" $ do
      it "returns absolute address + Y register" $ do
        let cpu = newCpu {registerY = 0x05, memory = [0x34, 0x12]}
        getOperandAddress cpu AbsoluteY `shouldBe` 0x1239

    describe "IndirectX" $ do
      it "returns dereferenced address + X register" $ do
        let cpu = newCpu {registerX = 0x05, memory = [0, 0, 0, 0, 0, 0x34, 0x12]}
        getOperandAddress cpu IndirectX `shouldBe` 0x1234

    -- TODO: There might be some problem with this
    describe "IndirectY" $ do
      it "returns dereferenced address + Y register" $ do
        let cpu = newCpu {registerY = 0x05, memory = [0, 0x34, 0x12]}
        getOperandAddress cpu IndirectY `shouldBe` 0x3405
