module NesEmu.Cpu where

import           Data.Bits
import           Data.Word

data Cpu = Cpu
  { registerA      :: !Word8,
    registerX      :: !Word8,
    status         :: !Word8,
    programCounter :: !Word16,
    memory         :: ![Word8]
  }
  deriving (Show)

newCpu :: Cpu
newCpu =
  Cpu
    { registerA = 0,
      registerX = 0,
      status = 0,
      programCounter = 0,
      memory = replicate 0xFFFF 0
    }

loadAndRun :: [Word8] -> Cpu
loadAndRun = run . loadProgram

loadProgram :: [Word8] -> Cpu
loadProgram pgm =
  Cpu
    { registerA = 0,
      registerX = 0,
      status = 0,
      programCounter = 0x8000,
      memory = replicate 0x8000 0 ++ pgm ++ replicate l 0
    }
  where
    l = 0xFFFF - length pgm - 0x8000

run :: Cpu -> Cpu
run cpu =
  case opcode of
    0x00 -> brk cpu
    0xAA -> run . incPc $ tax cpu
    0xE8 -> run . incPc $ inx cpu
    0xA9 -> run . incPc $ lda cpu (memoryRead cpu $ programCounter cpu + 1)
    _    -> undefined
  where
    opcode = memoryRead cpu (programCounter cpu)
    incPc a = a {programCounter = programCounter a + 1}

-- Set Flags
data Flag
  = Carry
  | Zero
  | Interrupt
  | Decimal
  | Nop1
  | Nop2
  | Overflow
  | Negative
  deriving (Enum)

setFlag :: Flag -> Bool -> Word8 -> Word8
setFlag flag val s = f s (fromEnum flag)
  where
    f = if val then setBit else clearBit

setZF :: Word8 -> Word8 -> Word8
setZF = setFlag Zero . (== 0)

setNF :: Word8 -> Word8 -> Word8
setNF = setFlag Negative . flip testBit (fromEnum Negative)

setVF :: Word8 -> Word8 -> Word8
setVF = setFlag Overflow . (== 0xFF)

-- Memory Access
memoryWrite :: Cpu -> Word16 -> Word8 -> Cpu
memoryWrite cpu addr val =
  cpu
    { memory = take addrI mem ++ [val] ++ drop (addrI + 1) mem
    }
  where
    mem = memory cpu
    addrI = fromIntegral addr :: Int

memoryRead :: Cpu -> Word16 -> Word8
memoryRead cpu addr = memory cpu !! (fromIntegral addr :: Int)

-- Operations
lda :: Cpu -> Word8 -> Cpu
lda cpu val =
  cpu
    { registerA = val,
      programCounter = programCounter cpu + 1,
      status = setZF val $ setNF val $ status cpu
    }

tax :: Cpu -> Cpu
tax cpu =
  cpu
    { registerX = result,
      status = setZF result $ setNF result $ status cpu
    }
  where
    result = registerA cpu

inx :: Cpu -> Cpu
inx cpu =
  cpu
    { registerX = result,
      status = setVF (registerX cpu) $ setZF result $ setNF result $ status cpu
    }
  where
    result = registerX cpu + 1

brk :: Cpu -> Cpu
brk cpu = cpu
