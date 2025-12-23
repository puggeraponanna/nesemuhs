module NesEmuTest.Cpu.Operations where

import           Data.Bits
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

    describe "AND" $ do
        it "performs bitwise AND on accumulator" $ do
            let cpu = loadAndRun newCpu [0xA9, 0xFF, 0x29, 0xAA, 0x00]
            registerA cpu `shouldBe` 0xAA

    describe "EOR" $ do
        it "performs bitwise XOR on accumulator" $ do
            let cpu = loadAndRun newCpu [0xA9, 0xFF, 0x49, 0xAA, 0x00]
            registerA cpu `shouldBe` 0x55

    describe "ORA" $ do
        it "performs bitwise OR on accumulator" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x55, 0x09, 0xAA, 0x00]
            registerA cpu `shouldBe` 0xFF

    describe "BIT" $ do
        it "sets Z, N, and V flags correctly" $ do
            let program = [0xA9, 0xC0, 0x85, 0x10, 0xA9, 0x01, 0x24, 0x10, 0x00]
            let cpu = loadAndRun newCpu program
            getFlag Zero cpu `shouldBe` True
            getFlag Negative cpu `shouldBe` True
            getFlag Overflow cpu `shouldBe` True

    describe "CMP" $ do
        it "sets carry if A >= M" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x10, 0xC9, 0x05, 0x00]
            getFlag Carry cpu `shouldBe` True
        it "sets zero if A == M" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x10, 0xC9, 0x10, 0x00]
            getFlag Zero cpu `shouldBe` True
        it "sets negative if A < M (and result has bit 7)" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x10, 0xC9, 0x20, 0x00]
            getFlag Negative cpu `shouldBe` True

    describe "ASL" $ do
        it "shifts accumulator left" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x80, 0x0A, 0x00]
            registerA cpu `shouldBe` 0x00
            getFlag Carry cpu `shouldBe` True

    describe "LSR" $ do
        it "shifts accumulator right" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x01, 0x4A, 0x00]
            registerA cpu `shouldBe` 0x00
            getFlag Carry cpu `shouldBe` True

    describe "ROL" $ do
        it "rotates accumulator left through carry" $ do
            let cpu = loadAndRun newCpu [0x38, 0xA9, 0x00, 0x2A, 0x00]
            registerA cpu `shouldBe` 0x01
            getFlag Carry cpu `shouldBe` False

    describe "ROR" $ do
        it "rotates accumulator right through carry" $ do
            let cpu = loadAndRun newCpu [0x38, 0xA9, 0x00, 0x6A, 0x00]
            registerA cpu `shouldBe` 0x80
            getFlag Carry cpu `shouldBe` False

    describe "Branches" $ do
        it "BNE takes branch if zero flag is clear" $ do
            let program = [0xA9, 0x01, 0xC9, 0x02, 0xD0, 0x03, 0x00, 0x00, 0x00, 0xA9, 0x05, 0x00]
            let cpu = loadAndRun newCpu program
            registerA cpu `shouldBe` 0x05

        it "BNE does not take branch if zero flag is set" $ do
            let program = [0xA9, 0x01, 0xC9, 0x01, 0xD0, 0x03, 0xA9, 0x05, 0x00]
            let cpu = loadAndRun newCpu program
            registerA cpu `shouldBe` 0x05

    describe "JMP" $ do
        it "jumps to absolute address" $ do
            let program = [0x4C, 0x05, 0x80, 0x00, 0x00, 0xA9, 0x05, 0x00]
            let cpu = loadAndRun newCpu program
            registerA cpu `shouldBe` 0x05
    describe "JSR/RTS" $ do
        it "calls a subroutine and returns" $ do
            let program = [0x20, 0x05, 0x80, 0x00, 0x00, 0xA9, 0x05, 0x60, 0x00]
            let cpu = loadAndRun newCpu program
            registerA cpu `shouldBe` 0x05

    describe "PHA/PLA" $ do
        it "pushes and pulls accumulator" $ do
            let program = [0xA9, 0x42, 0x48, 0xA9, 0x00, 0x68, 0x00]
            let cpu = loadAndRun newCpu program
            registerA cpu `shouldBe` 0x42

    describe "Transfer" $ do
        it "TAY transfers A to Y" $ do
            let cpu = loadAndRun newCpu [0xA9, 0x42, 0xA8, 0x00]
            registerY cpu `shouldBe` 0x42
        it "TYA transfers Y to A" $ do
            let cpu = loadAndRun newCpu [0xA0, 0x42, 0x98, 0x00]
            registerA cpu `shouldBe` 0x42
        it "TXA transfers X to A" $ do
            let cpu = loadAndRun newCpu [0xA2, 0x42, 0x8A, 0x00]
            registerA cpu `shouldBe` 0x42
        it "TSX/TXS transfers between X and SP" $ do
            let cpu = loadAndRun newCpu [0xA2, 0xEE, 0x9A, 0xA2, 0x00, 0xBA, 0x00]
            registerX cpu `shouldBe` 0xEE
            stackPointer cpu `shouldBe` 0xEE

    describe "Flags" $ do
        it "CLC clears carry" $ do
            let cpu = loadAndRun newCpu [0x38, 0x18, 0x00]
            getFlag Carry cpu `shouldBe` False
        it "SEC sets carry" $ do
            let cpu = loadAndRun newCpu [0x18, 0x38, 0x00]
            getFlag Carry cpu `shouldBe` True
    describe "PHP/PLP" $ do
        it "pushes and pulls status register" $ do
            let program = [0x38, 0x08, 0x18, 0x28, 0x00]
            let cpu = loadAndRun newCpu program
            getFlag Carry cpu `shouldBe` True

    describe "RTI" $ do
        it "returns from interrupt" $ do
            let program = [0xA9, 0x80, 0x48, 0xA9, 0x0F, 0x48, 0xA9, 0x42, 0x48, 0x40, 0, 0, 0, 0, 0, 0x85, 0x00, 0x00]
            let cpu = loadAndRun newCpu program
            status cpu `shouldBe` (0x42 .&. 0xEF) .|. 0x20
