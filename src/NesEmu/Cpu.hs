module NesEmu.Cpu where

import           Data.Bits
import           Data.Word

data Cpu = Cpu
  { registerA      :: !Word8,
    registerX      :: !Word8,
    registerY      :: !Word8,
    status         :: !Word8,
    programCounter :: !Word16,
    memory         :: ![Word8]
  }
  deriving (Show)

reset :: Cpu -> Cpu
reset cpu =
  cpu
    { registerA = 0,
      registerX = 0,
      registerY = 0,
      status = 0,
      programCounter = memoryRead16 cpu 0xFFFC
    }

newCpu :: Cpu
newCpu =
  Cpu
    { registerA = 0,
      registerX = 0,
      registerY = 0,
      status = 0,
      programCounter = 0,
      memory = replicate 0xFFFF 0
    }

loadAndRun :: Cpu -> [Word8] -> Cpu
loadAndRun cpu = run . loadProgram cpu

loadProgram :: Cpu -> [Word8] -> Cpu
loadProgram cpu pgm =
  let cpu' =
        cpu
          { memory = take 0x8000 (memory cpu) ++ pgm ++ drop l (memory cpu)
          }
   in reset $ memoryWrite16 cpu' 0xFFFC 0x8000
  where
    l = length pgm + 0x8000

run :: Cpu -> Cpu
run cpu =
  case opcode of
    0x00 -> brk (incPc 1 cpu)
    0xAA -> run . incPc 0 $ tax (incPc 1 cpu)
    0xE8 -> run . incPc 0 $ inx (incPc 1 cpu)
    0xA9 -> run . incPc 1 $ lda (incPc 1 cpu) Immediate
    0xA5 -> run . incPc 1 $ lda (incPc 1 cpu) ZeroPage
    0xB5 -> run . incPc 1 $ lda (incPc 1 cpu) ZeroPageX
    0xAD -> run . incPc 2 $ lda (incPc 1 cpu) Absolute
    0xBD -> run . incPc 2 $ lda (incPc 1 cpu) AbsoluteX
    0xB9 -> run . incPc 2 $ lda (incPc 1 cpu) AbsoluteY
    0xA1 -> run . incPc 1 $ lda (incPc 1 cpu) IndirectX
    0xB1 -> run . incPc 1 $ lda (incPc 1 cpu) IndirectY
    0x85 -> run . incPc 1 $ sta (incPc 1 cpu) ZeroPage
    0x95 -> run . incPc 1 $ sta (incPc 1 cpu) ZeroPageX
    0x8D -> run . incPc 2 $ sta (incPc 1 cpu) Absolute
    0x9D -> run . incPc 2 $ sta (incPc 1 cpu) AbsoluteX
    0x99 -> run . incPc 2 $ sta (incPc 1 cpu) AbsoluteY
    0x81 -> run . incPc 1 $ sta (incPc 1 cpu) IndirectX
    0x91 -> run . incPc 1 $ sta (incPc 1 cpu) IndirectY
    0x69 -> run . incPc 1 $ adc (incPc 1 cpu) Immediate
    _    -> undefined
  where
    opcode = memoryRead cpu (programCounter cpu)
    incPc i a = a {programCounter = programCounter a + i}

data AddressingMode
  = Immediate
  | ZeroPage
  | ZeroPageX
  | ZeroPageY
  | Absolute
  | AbsoluteX
  | AbsoluteY
  | IndirectX
  | IndirectY
  | NoneAddressing
  deriving (Show)

getOperandAddress :: Cpu -> AddressingMode -> Word16
getOperandAddress cpu Immediate = programCounter cpu
getOperandAddress cpu ZeroPage = fromIntegral $ memoryRead cpu (programCounter cpu)
getOperandAddress cpu Absolute = memoryRead16 cpu (programCounter cpu)
getOperandAddress cpu ZeroPageX =
  let pos = memoryRead cpu (programCounter cpu)
   in fromIntegral pos + fromIntegral (registerX cpu)
getOperandAddress cpu ZeroPageY =
  let pos = memoryRead cpu (programCounter cpu)
   in fromIntegral pos + fromIntegral (registerY cpu)
getOperandAddress cpu AbsoluteX =
  let base = memoryRead16 cpu (programCounter cpu)
   in base + fromIntegral (registerX cpu)
getOperandAddress cpu AbsoluteY =
  let base = memoryRead16 cpu (programCounter cpu)
   in base + fromIntegral (registerY cpu)
getOperandAddress cpu IndirectX =
  let base = memoryRead cpu (programCounter cpu)
      ptr = base + registerX cpu
      lo = fromIntegral $ memoryRead cpu (fromIntegral ptr)
      hi = fromIntegral $ memoryRead cpu (fromIntegral ptr + 1)
   in (hi `shiftL` 8) + lo
getOperandAddress cpu IndirectY =
  let base = memoryRead cpu (programCounter cpu)
      lo = memoryRead cpu (fromIntegral base)
      hi = memoryRead cpu (fromIntegral base + 1)
      deref_base = (fromIntegral hi `shiftL` 8) + fromIntegral lo
   in deref_base + fromIntegral (registerY cpu)
getOperandAddress _ NoneAddressing = undefined

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

getFlag :: Cpu -> Flag -> Bool
getFlag cpu flag = testBit (status cpu) (fromEnum flag)

setZF :: Word8 -> Word8 -> Word8
setZF = setFlag Zero . (== 0)

setNF :: Word8 -> Word8 -> Word8
setNF = setFlag Negative . flip testBit (fromEnum Negative)

setVF :: Word8 -> Word8 -> Word8
setVF = setFlag Overflow . (== 0xFF)

setCF :: Bool -> Word8 -> Word8
setCF = setFlag Carry

-- Memory Access
memoryWrite :: Cpu -> Word16 -> Word8 -> Cpu
memoryWrite cpu addr val =
  cpu
    { memory = take addr' mem ++ [val] ++ drop (addr' + 1) mem
    }
  where
    mem = memory cpu
    addr' = fromIntegral addr :: Int

memoryWrite16 :: Cpu -> Word16 -> Word16 -> Cpu
memoryWrite16 cpu addr val = memoryWrite (memoryWrite cpu addr lo) (addr + 1) hi
  where
    lo = fromIntegral $ (val .&. 0xFF)
    hi = fromIntegral $ val `shiftR` 8

memoryRead :: Cpu -> Word16 -> Word8
memoryRead cpu addr = memory cpu !! fromIntegral addr

memoryRead16 :: Cpu -> Word16 -> Word16
memoryRead16 cpu addr = (hi `shiftL` 8) + lo
  where
    lo = fromIntegral $ memoryRead cpu addr
    hi = fromIntegral $ memoryRead cpu (addr + 1)

-- Operations
lda :: Cpu -> AddressingMode -> Cpu
lda cpu addrMode =
  let addr = getOperandAddress cpu addrMode
      val = memoryRead cpu addr
   in cpu
        { registerA = val,
          status = setZF val $ setNF val $ status cpu
        }

sta :: Cpu -> AddressingMode -> Cpu
sta cpu addrMode =
  let addr = getOperandAddress cpu addrMode
      val = registerA cpu
   in memoryWrite cpu addr val

tax :: Cpu -> Cpu
tax cpu =
  cpu
    { registerX = result,
      status = setZF result $ setNF result $ status cpu
    }
  where
    result = registerA cpu

adc :: Cpu -> AddressingMode -> Cpu
adc cpu addrMode =
  cpu
    { registerA = fromIntegral result .&. 0xFF,
      status = setCF cf $ setVF overflow $ setZF result' $ setNF result' $ status cpu
    }
  where
    addr = getOperandAddress cpu addrMode
    val = memoryRead cpu addr
    carry = if getFlag cpu Carry then 1 else 0
    result = fromIntegral (registerA cpu) + fromIntegral val + carry :: Int
    result' = fromIntegral (result .&. 0xFF)
    cf = getFlag cpu Zero || result > 0xFF
    overflow = if (complement (xor (registerA cpu) val) .&. xor (registerA cpu) result') .&. 0x80 > 0 then 0xFF else 0x00

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
