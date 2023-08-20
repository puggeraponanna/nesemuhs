module NesEmu.Cpu where

import           Data.Word
import           NesEmu.Cpu.Memory
import           NesEmu.Cpu.Opcodes
import           NesEmu.Cpu.Operations
import           NesEmu.Cpu.Types

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
    BRK          -> brk (incPc 1 cpu)
    TAX          -> run . incPc 0 $ tax (incPc 1 cpu)
    INX          -> run . incPc 0 $ inx (incPc 1 cpu)
    LDAImmediate -> run . incPc 1 $ lda (incPc 1 cpu) Immediate
    LDAZeroPage  -> run . incPc 1 $ lda (incPc 1 cpu) ZeroPage
    LDAZeroPageX -> run . incPc 1 $ lda (incPc 1 cpu) ZeroPageX
    LDAAbsolute  -> run . incPc 2 $ lda (incPc 1 cpu) Absolute
    LDAAbsoluteX -> run . incPc 2 $ lda (incPc 1 cpu) AbsoluteX
    LDAAbsoluteY -> run . incPc 2 $ lda (incPc 1 cpu) AbsoluteY
    LDAIndirectX -> run . incPc 1 $ lda (incPc 1 cpu) IndirectX
    LDAIndirectY -> run . incPc 1 $ lda (incPc 1 cpu) IndirectY
    LDXImmediate -> run . incPc 1 $ ldx (incPc 1 cpu) Immediate
    LDXZeroPage  -> run . incPc 1 $ ldx (incPc 1 cpu) ZeroPage
    LDXZeroPageY -> run . incPc 1 $ ldx (incPc 1 cpu) ZeroPageY
    LDXAbsolute  -> run . incPc 2 $ ldx (incPc 1 cpu) Absolute
    LDXAbsoluteY -> run . incPc 2 $ ldx (incPc 1 cpu) AbsoluteY
    STAZeroPage  -> run . incPc 1 $ sta (incPc 1 cpu) ZeroPage
    STAZeroPageX -> run . incPc 1 $ sta (incPc 1 cpu) ZeroPageX
    STAAbsolute  -> run . incPc 2 $ sta (incPc 1 cpu) Absolute
    STAAbsoluteX -> run . incPc 2 $ sta (incPc 1 cpu) AbsoluteX
    STAAbsoluteY -> run . incPc 2 $ sta (incPc 1 cpu) AbsoluteY
    STAIndirectX -> run . incPc 1 $ sta (incPc 1 cpu) IndirectX
    STAIndirectY -> run . incPc 1 $ sta (incPc 1 cpu) IndirectY
    ADCImmediate -> run . incPc 1 $ adc (incPc 1 cpu) Immediate
    ADCZeroPage  -> run . incPc 1 $ adc (incPc 1 cpu) ZeroPage
    ADCZeroPageX -> run . incPc 1 $ adc (incPc 1 cpu) ZeroPageX
    ADCAbsolute  -> run . incPc 2 $ adc (incPc 1 cpu) Absolute
    ADCAbsoluteX -> run . incPc 2 $ adc (incPc 1 cpu) AbsoluteX
    ADCAbsoluteY -> run . incPc 2 $ adc (incPc 1 cpu) AbsoluteY
    ADCIndirectX -> run . incPc 1 $ adc (incPc 1 cpu) IndirectX
    ADCIndirectY -> run . incPc 1 $ adc (incPc 1 cpu) IndirectY
    SBCImmediate -> run . incPc 1 $ sbc (incPc 1 cpu) Immediate
    SBCZeroPage  -> run . incPc 1 $ sbc (incPc 1 cpu) ZeroPage
    SBCZeroPageX -> run . incPc 1 $ sbc (incPc 1 cpu) ZeroPageX
    SBCAbsolute  -> run . incPc 2 $ sbc (incPc 1 cpu) Absolute
    SBCAbsoluteX -> run . incPc 2 $ sbc (incPc 1 cpu) AbsoluteX
    SBCAbsoluteY -> run . incPc 2 $ sbc (incPc 1 cpu) AbsoluteY
    SBCIndirectX -> run . incPc 1 $ sbc (incPc 1 cpu) IndirectX
    SBCIndirectY -> run . incPc 1 $ sbc (incPc 1 cpu) IndirectY
    _            -> error "Unknown opcode"
  where
    opcode = memoryRead cpu (programCounter cpu)
    incPc i a = a {programCounter = programCounter a + i}

incPc :: Cpu -> Word16 -> Cpu
incPc cpu i = cpu {programCounter = programCounter cpu + i}

-- executeOpcode :: Cpu -> OpCode -> Cpu
-- executeOpcode cpu opBrk = brk (incPc cpu 1)
-- executeOpcode cpu 0xAA -> incPc 0 $ tax (incPc 1 cpu)
-- executeOpcode cpu 0xE8 -> incPc 0 $ inx (incPc 1 cpu)
-- executeOpcode cpu 0xA9 -> incPc 1 $ lda (incPc 1 cpu) Immediate
-- executeOpcode cpu 0xA5 -> incPc 1 $ lda (incPc 1 cpu) ZeroPage
-- executeOpcode cpu 0xB5 -> incPc 1 $ lda (incPc 1 cpu) ZeroPageX
-- executeOpcode cpu 0xAD -> incPc 2 $ lda (incPc 1 cpu) Absolute
-- executeOpcode cpu 0xBD -> incPc 2 $ lda (incPc 1 cpu) AbsoluteX
-- executeOpcode cpu 0xB9 -> incPc 2 $ lda (incPc 1 cpu) AbsoluteY
-- executeOpcode cpu 0xA1 -> incPc 1 $ lda (incPc 1 cpu) IndirectX
-- executeOpcode cpu 0xB1 -> incPc 1 $ lda (incPc 1 cpu) IndirectY
-- executeOpcode cpu 0x85 -> incPc 1 $ sta (incPc 1 cpu) ZeroPage
-- executeOpcode cpu 0x95 -> incPc 1 $ sta (incPc 1 cpu) ZeroPageX
-- executeOpcode cpu 0x8D -> incPc 2 $ sta (incPc 1 cpu) Absolute
-- executeOpcode cpu 0x9D -> incPc 2 $ sta (incPc 1 cpu) AbsoluteX
-- executeOpcode cpu 0x99 -> incPc 2 $ sta (incPc 1 cpu) AbsoluteY
-- executeOpcode cpu 0x81 -> incPc 1 $ sta (incPc 1 cpu) IndirectX
-- executeOpcode cpu 0x91 -> incPc 1 $ sta (incPc 1 cpu) IndirectY
-- executeOpcode cpu 0x69 -> incPc 1 $ adc (incPc 1 cpu) Immediate
-- executeOpcode cpu 0x65 -> incPc 1 $ adc (incPc 1 cpu) ZeroPage
-- executeOpcode cpu 0x75 -> incPc 1 $ adc (incPc 1 cpu) ZeroPageX
-- executeOpcode cpu 0x6D -> incPc 2 $ adc (incPc 1 cpu) Absolute
-- executeOpcode cpu 0x7D -> incPc 2 $ adc (incPc 1 cpu) AbsoluteX
-- executeOpcode cpu 0x79 -> incPc 2 $ adc (incPc 1 cpu) AbsoluteY
-- executeOpcode cpu 0x61 -> incPc 1 $ adc (incPc 1 cpu) IndirectX
-- executeOpcode cpu 0x71 -> incPc 1 $ adc (incPc 1 cpu) IndirectY
-- executeOpcode cpu 0xE9 -> incPc 1 $ sbc (incPc 1 cpu) Immediate
-- executeOpcode cpu 0xE5 -> incPc 1 $ sbc (incPc 1 cpu) ZeroPage
-- executeOpcode cpu 0xF5 -> incPc 1 $ sbc (incPc 1 cpu) ZeroPageX
-- executeOpcode cpu 0xED -> incPc 2 $ sbc (incPc 1 cpu) Absolute
-- executeOpcode cpu 0xFD -> incPc 2 $ sbc (incPc 1 cpu) AbsoluteX
-- executeOpcode cpu 0xF9 -> incPc 2 $ sbc (incPc 1 cpu) AbsoluteY
-- executeOpcode cpu 0xE1 -> incPc 1 $ sbc (incPc 1 cpu) IndirectX
-- executeOpcode cpu 0xF1 -> incPc 1 $ sbc (incPc 1 cpu) IndirectY
