module CpuSpec (spec) where

import           Data.Bits
import           GHC.IO.Handle.Text (memcpy)
import           NesEmu.Cpu
import           Test.Hspec

spec :: Spec
spec =
  describe "CPU" $ do
    describe "LDA IMM" $ do
      it "Should load data to Register A from Immediate address" $ do
        let cpu = loadAndRun [0xA9, 0x11, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        registerA cpu `shouldBe` 0x11
        zf `shouldBe` False
        nf `shouldBe` False

      it "Should set the Zero flag if result is zero" $ do
        let cpu = loadAndRun [0xA9, 0x00, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        zf `shouldBe` True
        nf `shouldBe` False

      it "Should set the Negative flag if result is negative" $ do
        let cpu = loadAndRun [0xA9, 0x80, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        zf `shouldBe` False
        nf `shouldBe` True

    describe "TAX" $ do
      it "Should transfer data from Register A to Register X" $ do
        let cpu = loadAndRun [0xA9, 0x45, 0xAA, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        registerX cpu `shouldBe` 0x45
        zf `shouldBe` False
        nf `shouldBe` False

      it "Should set the Zero flag if result is zero" $ do
        let cpu = loadAndRun [0xAA, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        zf `shouldBe` True
        nf `shouldBe` False

      it "Should set the Negative flag if result is negative" $ do
        let cpu = loadAndRun [0xA9, 0x91, 0xAA, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        zf `shouldBe` False
        nf `shouldBe` True

    describe "INX" $ do
      it "Should increment the value in Register X" $ do
        let cpu = loadAndRun [0xA9, 0x11, 0xAA, 0xE8, 0x00]

        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7
        let vf = testBit (status cpu) 6

        registerX cpu `shouldBe` 0x12
        zf `shouldBe` False
        nf `shouldBe` False
        vf `shouldBe` False

      it "Should increment the value in Register X and set the Overflow flag" $ do
        let cpu = loadAndRun [0xA9, 0xFF, 0xAA, 0xE8, 0x00]

        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7
        let vf = testBit (status cpu) 6

        registerX cpu `shouldBe` 0x00
        zf `shouldBe` True
        nf `shouldBe` False
        vf `shouldBe` True

    describe "Memory Access" $ do
      it "SHould successfully write into memory" $ do
        let cpu = memoryWrite newCpu 0x01 0xAD

        memory cpu !! 0x01 `shouldBe` 0xAD
        length (memory cpu) `shouldBe` 0xFFFF

      it "Should successfully read from memory" $ do
        let cpu =
              newCpu
                { memory = [0x12, 0x34]
                }

        let dt = memoryRead cpu 0x0000
        dt `shouldBe` 0x12
