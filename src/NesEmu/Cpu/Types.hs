module NesEmu.Cpu.Types where

import           Data.Word (Word16, Word8)

data Cpu = Cpu
  { registerA      :: !Word8,
    registerX      :: !Word8,
    registerY      :: !Word8,
    status         :: !Word8,
    programCounter :: !Word16,
    memory         :: ![Word8]
  }
  deriving (Show)
