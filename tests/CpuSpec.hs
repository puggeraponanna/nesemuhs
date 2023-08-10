module CpuSpec (spec) where

import           Data.Bits
import           NesEmu.Cpu
import           Test.Hspec

spec :: Spec
spec =
  describe "CPU" $ do
    describe "LDA" $ do
      describe "Immediate" $ do
        it "Should load data to Register A from Immediate address" $ do
          let cpu = loadAndRun newCpu [0xA9, 0x11, 0x00]

          registerA cpu `shouldBe` 0x11
          getFlag cpu ZeroF `shouldBe` False
          getFlag cpu NegativeF `shouldBe` False

        it "Should set the Zero flag if result is zero" $ do
          let cpu = loadAndRun newCpu [0xA9, 0x00, 0x00]

          getFlag cpu ZeroF `shouldBe` True
          getFlag cpu NegativeF `shouldBe` False

        it "Should set the Negative flag if result is negative" $ do
          let cpu = loadAndRun newCpu [0xA9, 0x80, 0x00]

          getFlag cpu ZeroF `shouldBe` False
          getFlag cpu NegativeF `shouldBe` True

      describe "ZeroPage" $ do
        it "Should load data to Register A from Zero Page address" $ do
          let initCpu = memoryWrite newCpu 0x10 0x55
          let cpu = loadAndRun initCpu [0xA5, 0x10, 0x00]

          registerA cpu `shouldBe` 0x55
          getFlag cpu ZeroF `shouldBe` False
          getFlag cpu NegativeF `shouldBe` False

      describe "ZeroPage X offset" $ do
        it "Should load data to Register A from Zero Page address with X offset" $ do
          let initCpu = memoryWrite newCpu 0x12 0x55
          let cpu = loadAndRun initCpu [0xA9, 0x02, 0xAA, 0xB5, 0x10, 0x00]

          registerA cpu `shouldBe` 0x55
          getFlag cpu ZeroF `shouldBe` False
          getFlag cpu NegativeF `shouldBe` False

      describe "Absolute" $ do
        it "Should load data to Register A from Absolute address" $ do
          let initCpu = memoryWrite newCpu 0x12 0x55
          let cpu = loadAndRun initCpu [0xAD, 0x12, 0x00]

          registerA cpu `shouldBe` 0x55
          getFlag cpu ZeroF `shouldBe` False
          getFlag cpu NegativeF `shouldBe` False

    describe "STA" $ do
      describe "ZeroPage" $ do
        it "Should write data from Register A to Zero Page address" $ do
          let cpu = loadAndRun newCpu [0xA9, 0x55, 0x85, 0x10, 0x00]

          memoryRead cpu 0x10 `shouldBe` 0x55

    describe "TAX" $ do
      it "Should transfer data from Register A to Register X" $ do
        let cpu = loadAndRun newCpu [0xA9, 0x45, 0xAA, 0x00]

        registerX (tax cpu) `shouldBe` 0x45
        getFlag cpu ZeroF `shouldBe` False
        getFlag cpu NegativeF `shouldBe` False

      it "Should set the Zero flag if result is zero" $ do
        let cpu = loadAndRun newCpu [0xAA, 0x00]

        getFlag cpu ZeroF `shouldBe` True
        getFlag cpu NegativeF `shouldBe` False

      it "Should set the Negative flag if result is negative" $ do
        let cpu = loadAndRun newCpu [0xA9, 0x91, 0xAA, 0x00]

        getFlag cpu ZeroF `shouldBe` False
        getFlag cpu NegativeF `shouldBe` True

    describe "INX" $ do
      it "Should increment the value in Register X" $ do
        let cpu = loadAndRun newCpu [0xA9, 0x11, 0xAA, 0xE8, 0x00]

        registerX cpu `shouldBe` 0x12
        getFlag cpu ZeroF `shouldBe` False
        getFlag cpu NegativeF `shouldBe` False
        getFlag cpu OverflowF `shouldBe` False

      it "Should increment the value in Register X and set the Overflow flag" $ do
        let cpu = loadAndRun newCpu [0xA9, 0xFF, 0xAA, 0xE8, 0x00]

        registerX cpu `shouldBe` 0x00
        getFlag cpu ZeroF `shouldBe` True
        getFlag cpu NegativeF `shouldBe` False
        getFlag cpu OverflowF `shouldBe` True

    describe "ADC" $ do
      it "Should add two positive numbers" $ do
        let cpu = loadAndRun newCpu [0xA9, 0x05, 0x69, 0x03, 0x00]

        registerA cpu `shouldBe` 0x08
        getFlag cpu CarryF `shouldBe` False
        getFlag cpu ZeroF `shouldBe` False
        getFlag cpu NegativeF `shouldBe` False
        getFlag cpu OverflowF `shouldBe` False

      it "Should add a negative number to a positive number" $ do
        let cpu = loadAndRun newCpu [0xA9, 0x80, 0x69, 0xFF, 0x00]

        registerA cpu `shouldBe` 0x7F
        getFlag cpu CarryF `shouldBe` True
        getFlag cpu ZeroF `shouldBe` False
        getFlag cpu NegativeF `shouldBe` False
        getFlag cpu OverflowF `shouldBe` True

      it "Should add a positive number to a negative number" $ do
        let cpu = loadAndRun newCpu [0xA9, 0xFF, 0x69, 0x80, 0x00]

        registerA cpu `shouldBe` 0x7F
        getFlag cpu CarryF `shouldBe` True
        getFlag cpu ZeroF `shouldBe` False
        getFlag cpu NegativeF `shouldBe` False
        getFlag cpu OverflowF `shouldBe` True

      it "adds two negative numbers" $ do
        let cpu = loadAndRun newCpu [0xA9, 0x81, 0x69, 0xFF, 0x00]

        registerA cpu `shouldBe` 0x80
        getFlag cpu CarryF `shouldBe` True
        getFlag cpu ZeroF `shouldBe` False
        getFlag cpu NegativeF `shouldBe` True
        getFlag cpu OverflowF `shouldBe` False
