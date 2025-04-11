module NesEmuTest.Cpu.Operations where

import           NesEmu.Cpu
import           NesEmu.Cpu.Flags
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

    describe "LDY" $ do
        it "loads value into Y register" $ do
            let cpu = loadAndRun newCpu [0xA0, 0x42]
            registerY cpu `shouldBe` 0x42

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

    describe "DEX" $ do
        it "decrements the X register" $ do
            let cpu = loadAndRun newCpu [0xA2, 0x02, 0xCA]
            registerX cpu `shouldBe` 0x01

        it "sets the Zero flag if result is zero" $ do
            let cpu = loadAndRun newCpu [0xA2, 0x01, 0xCA]
            getFlag Zero cpu `shouldBe` True

        it "sets the Negative flag if result is negative" $ do
            let cpu = loadAndRun newCpu [0xA2, 0x00, 0xCA]
            getFlag Negative cpu `shouldBe` True

    describe "DEY" $ do
        it "decrements the Y register" $ do
            let cpu = loadAndRun newCpu [0xA0, 0x02, 0x88]
            registerY cpu `shouldBe` 0x01

        it "sets the Zero flag if result is zero" $ do
            let cpu = loadAndRun newCpu [0xA0, 0x01, 0x88]
            getFlag Zero cpu `shouldBe` True

        it "sets the Negative flag if result is negative" $ do
            let cpu = loadAndRun newCpu [0xA0, 0x00, 0x88]
            getFlag Negative cpu `shouldBe` True

    describe "INC" $ do
        it "increments a memory location" $ do
            let cpu = loadAndRun (memoryWrite newCpu 0x10 0x01) [0xE6, 0x10]
            memoryRead cpu 0x10 `shouldBe` 0x02

        it "sets the Zero flag if result is zero" $ do
            let cpu = loadAndRun (memoryWrite newCpu 0x10 0xFF) [0xE6, 0x10]
            getFlag Zero cpu `shouldBe` True

        it "sets the Negative flag if result is negative" $ do
            let cpu = loadAndRun (memoryWrite newCpu 0x10 0x7F) [0xE6, 0x10]
            getFlag Negative cpu `shouldBe` True

    describe "DEC" $ do
        it "decrements a memory location" $ do
            let cpu = loadAndRun (memoryWrite newCpu 0x10 0x02) [0xC6, 0x10]
            memoryRead cpu 0x10 `shouldBe` 0x01

        it "sets the Zero flag if result is zero" $ do
            let cpu = loadAndRun (memoryWrite newCpu 0x10 0x01) [0xC6, 0x10]
            getFlag Zero cpu `shouldBe` True

        it "sets the Negative flag if result is negative" $ do
            let cpu = loadAndRun (memoryWrite newCpu 0x10 0x00) [0xC6, 0x10]
            getFlag Negative cpu `shouldBe` True
