module NesEmu.Cpu where

import           Data.Bits
import           Data.Word (Word16, Word8)

data Cpu = Cpu
  { registerA      :: Word8,
    registerX      :: Word8,
    status         :: Word8,
    programCounter :: Word16
  }
  deriving (Show)

newCpu :: Cpu
newCpu =
  Cpu
    { registerA = 0,
      registerX = 0,
      status = 0,
      programCounter = 0
    }

interpret :: Cpu -> [Word8] -> Cpu
interpret cpu []              = cpu
interpret cpu (0x00 : ys)     = interpret (brk cpu) []
interpret cpu (0xAA : ys)     = interpret (tax cpu) ys
interpret cpu (0xE8 : ys)     = interpret (inx cpu) ys
interpret cpu (0xA9 : y : ys) = interpret (lda cpu y) ys
interpret _ _                 = undefined

setZF :: Word8 -> Word8 -> Word8
setZF result status = if result == 0 then setBit status 1 else clearBit status 1

setNF :: Word8 -> Word8 -> Word8
setNF result status = if testBit result 7 then setBit status 7 else clearBit status 7

setVF :: Word8 -> Word8 -> Word8
setVF result status = if result == 0xFF then setBit status 6 else clearBit status 6

lda :: Cpu -> Word8 -> Cpu
lda cpu val =
  cpu
    { registerA = val,
      programCounter = programCounter cpu + 1,
      status = setZF val $ setNF val (status cpu)
    }

tax :: Cpu -> Cpu
tax cpu =
  cpu
    { registerX = result,
      status = setZF result $ setNF result (status cpu)
    }
  where
    result = registerA cpu

inx :: Cpu -> Cpu
inx cpu =
  cpu
    { registerX = result,
      status = setVF (registerX cpu) $ setZF result $ setNF result (status cpu)
    }
  where
    result = registerX cpu + 1

brk :: Cpu -> Cpu
brk cpu = cpu
