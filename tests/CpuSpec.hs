module CpuSpec (spec) where

import           Data.Bits
import           NesEmu.Cpu
import           Test.Hspec

spec :: Spec
spec =
  describe "CPU" $ do
    describe "LDA IMM" $ do
      it "Should load data to Register A from Immediate address" $ do
        let cpu = interpret newCpu [0xA9, 0x11, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        registerA cpu `shouldBe` 0x11
        zf `shouldBe` False
        nf `shouldBe` False

      it "Should set the Zero flag if result is zero" $ do
        let cpu = interpret newCpu [0xA9, 0x00, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        zf `shouldBe` True
        nf `shouldBe` False

      it "Should set the Negative flag if result is negative" $ do
        let cpu = interpret newCpu [0xA9, 0x80, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        zf `shouldBe` False
        nf `shouldBe` True

    describe "TAX" $ do
      it "Should transfer data from Register A to Register X" $ do
        let cpu =
              newCpu
                { registerA = 0x45
                }
        let cpu' = interpret cpu [0xAA, 0x00]
        let zf = testBit (status cpu) 1
        let nf = testBit (status cpu) 7

        registerX cpu' `shouldBe` 0x45
        zf `shouldBe` False
        nf `shouldBe` False

      it "Should set the Zero flag if result is zero" $ do
        let cpu =
              newCpu
                { registerA = 0x00
                }
        let cpu' = interpret cpu [0xAA, 0x00]
        let zf = testBit (status cpu') 1
        let nf = testBit (status cpu') 7

        zf `shouldBe` True
        nf `shouldBe` False

      it "Should set the Negative flag if result is negative" $ do
        let cpu =
              newCpu
                { registerA = 0x91
                }
        let cpu' = interpret cpu [0xAA, 0x00]
        let zf = testBit (status cpu') 1
        let nf = testBit (status cpu') 7

        zf `shouldBe` False
        nf `shouldBe` True

    describe "INX" $ do
      it "Should increment the value in Register X" $ do
        let cpu =
              newCpu
                { registerX = 0x11
                }
        let cpu' = interpret cpu [0xE8, 0x00]

        let zf = testBit (status cpu') 1
        let nf = testBit (status cpu') 7
        let vf = testBit (status cpu') 6

        registerX cpu' `shouldBe` 0x12
        zf `shouldBe` False
        nf `shouldBe` False
        vf `shouldBe` False

      it "Should increment the value in Register X and set the Overflow flag" $ do
        let cpu =
              newCpu
                { registerX = 0xFF
                }
        let cpu' = interpret cpu [0xE8, 0x00]

        let zf = testBit (status cpu') 1
        let nf = testBit (status cpu') 7
        let vf = testBit (status cpu') 6

        registerX cpu' `shouldBe` 0x00
        zf `shouldBe` True
        nf `shouldBe` False
        vf `shouldBe` True
