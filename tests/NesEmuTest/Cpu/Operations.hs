module NesEmuTest.Cpu.Operations where

import           NesEmu.Cpu
import           NesEmu.Cpu.Memory
import           NesEmu.Cpu.Operations
import           NesEmu.Cpu.Types
import           Test.Hspec

spec :: Spec
spec = do
    describe "LDA" $ do
        it "loads value into accumulator" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x42]
            registerA cpu `shouldBe` 0x42

    describe "LDX" $ do
        it "loads value into X register" $ do
            let cpu = loadAndRun newCpu [0xA2, 0x42]
            registerX cpu `shouldBe` 0x42

    describe "STA" $ do
        it "stores accumulator value in memory" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x42, 0x85, 0x10]
            memoryRead cpu 0x10 `shouldBe` 0x42

    describe "TAX" $ do
        it "transfers accumulator to X register" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x42, 0xAA]
            registerX cpu `shouldBe` 0x42

    describe "INX" $ do
        it "increments X register" $ do
            let cpu = loadAndRun newCpu [0xA2, 0x40, 0xE8]
            registerX cpu `shouldBe` 0x41

    describe "ADC" $ do
        it "adds value to accumulator with carry" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x40, 0x69, 0x02]
            registerA cpu `shouldBe` 0x42
