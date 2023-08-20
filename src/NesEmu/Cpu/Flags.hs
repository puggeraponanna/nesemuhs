module NesEmu.Cpu.Flags where

import           Data.Bits
import           Data.Word
import           NesEmu.Cpu.Types

data Flag = Carry | Zero | Interrupt | Decimal | Break | Unused | Overflow | Negative
  deriving (Enum)

setFlag :: Flag -> Bool -> Cpu -> Cpu
setFlag flag True cpu  = cpu {status = setBit (status cpu) (fromEnum flag)}
setFlag flag False cpu = cpu {status = clearBit (status cpu) (fromEnum flag)}

getFlag :: Flag -> Cpu -> Bool
getFlag flag cpu = testBit (status cpu) (fromEnum flag)

setZF :: Word8 -> Cpu -> Cpu
setZF result = setFlag Zero (result == 0)

setNF :: Word8 -> Cpu -> Cpu
setNF result = setFlag Negative (testBit result 7)

setCF :: Bool -> Cpu -> Cpu
setCF = setFlag Carry

setVF :: Word8 -> Word8 -> Word8 -> Cpu -> Cpu
setVF a b c = setFlag Overflow ((complement (a `xor` b) .&. (a `xor` c) .&. 0x80) == 0x80)
