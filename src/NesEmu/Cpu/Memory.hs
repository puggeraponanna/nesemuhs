module NesEmu.Cpu.Memory where

import           Data.Bits
import           Data.Word        (Word16, Word8)
import           NesEmu.Cpu.Types

data AddressingMode
    = Immediate
    | ZeroPage
    | ZeroPageX
    | ZeroPageY
    | Absolute
    | AbsoluteX
    | AbsoluteY
    | Relative
    | Indirect
    | IndirectX
    | IndirectY
    | Accumulator
    | NoneAddressing
    deriving (Show)

getOperandAddress :: Cpu -> AddressingMode -> Word16
getOperandAddress cpu Immediate = programCounter cpu
getOperandAddress cpu ZeroPage =
    fromIntegral $
        memoryRead cpu (programCounter cpu)
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
getOperandAddress _ Indirect = 0
getOperandAddress _ Relative = 0
getOperandAddress _ Accumulator = 0
getOperandAddress _ NoneAddressing = undefined

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
    lo = fromIntegral (val .&. 0xFF)
    hi = fromIntegral $ val `shiftR` 8

memoryRead :: Cpu -> Word16 -> Word8
memoryRead cpu addr = memory cpu !! fromIntegral addr

memoryRead16 :: Cpu -> Word16 -> Word16
memoryRead16 cpu addr = (hi `shiftL` 8) + lo
  where
    lo = fromIntegral $ memoryRead cpu addr
    hi = fromIntegral $ memoryRead cpu (addr + 1)
