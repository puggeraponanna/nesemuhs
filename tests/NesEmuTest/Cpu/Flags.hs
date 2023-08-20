module NesEmuTest.Cpu.Flags where

import           NesEmu.Cpu
import           NesEmu.Cpu.Flags
import           Test.Hspec

spec :: Spec
spec = do
  describe "setFlag" $ do
    it "sets the specified flag bit" $ do
      let cpu = setFlag Carry True newCpu
      getFlag Carry cpu `shouldBe` True

  describe "getFlag" $ do
    it "returns True if the flag is set" $ do
      let cpu = setFlag Zero True newCpu
      getFlag Zero cpu `shouldBe` True

    it "returns False if the flag is cleared" $ do
      let cpu = setFlag Zero False newCpu
      getFlag Zero cpu `shouldBe` False

  describe "setZF" $ do
    it "sets the Z flag if result is 0" $ do
      let cpu = setZF 0 newCpu
      getFlag Zero cpu `shouldBe` True

    it "clears the Z flag if result is not 0" $ do
      let cpu = setZF 1 newCpu
      getFlag Zero cpu `shouldBe` False

  describe "setNF" $ do
    it "sets the N flag if bit 7 of result is 1" $ do
      let cpu = setNF 0x80 newCpu
      getFlag Negative cpu `shouldBe` True

    it "clears the N flag if bit 7 of result is 0" $ do
      let cpu = setNF 0x00 newCpu
      getFlag Negative cpu `shouldBe` False
