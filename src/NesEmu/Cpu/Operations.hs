module NesEmu.Cpu.Operations (getOperation, getCycleCount) where

import           Data.Word
import           NesEmu.Cpu.Flags
import           NesEmu.Cpu.Memory
import           NesEmu.Cpu.Opcodes
import           NesEmu.Cpu.Types

type Operation = Cpu -> Cpu

lda :: AddressingMode -> Cpu -> Cpu
lda addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        result = memoryRead cpu addr
        status' = status $ setZF result $ setNF result cpu
     in cpu
            { registerA = result
            , status = status'
            }

ldx :: AddressingMode -> Cpu -> Cpu
ldx addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        result = memoryRead cpu addr
        status' = status $ setZF result $ setNF result cpu
     in cpu
            { registerX = result
            , status = status'
            }

ldy :: AddressingMode -> Cpu -> Cpu
ldy addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        result = memoryRead cpu addr
        status' = status $ setZF result $ setNF result cpu
     in cpu
            { registerY = result
            , status = status'
            }

sta :: AddressingMode -> Cpu -> Cpu
sta addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        val = registerA cpu
     in memoryWrite cpu addr val

tax :: AddressingMode -> Cpu -> Cpu
tax _ cpu =
    let result = registerA cpu
        status' = status $ setZF result $ setNF result cpu
     in cpu
            { registerX = result
            , status = status'
            }

adc :: AddressingMode -> Cpu -> Cpu
adc addrMode cpu =
    let a = registerA cpu
        b = memoryRead cpu (getOperandAddress cpu addrMode)
        c = if getFlag Carry cpu then 1 else 0
        result = a + b + c
        status' = status $ setZF result $ setNF result $ setCF (result > 255) $ setVF a b c cpu
     in cpu
            { registerA = result
            , status = status'
            }

sbc :: AddressingMode -> Cpu -> Cpu
sbc addrMode cpu =
    let a = registerA cpu
        b = memoryRead cpu (getOperandAddress cpu addrMode)
        c = if getFlag Carry cpu then 1 else 0
        result = a - b - c
        status' = status $ setZF result $ setNF result $ setCF (result > 255) $ setVF a b c cpu
     in cpu
            { registerA = result
            , status = status'
            }

inx :: AddressingMode -> Cpu -> Cpu
inx _ cpu =
    let result = registerX cpu + 1
        status' = status $ setZF result $ setNF result $ setVF result 1 0 cpu
     in cpu
            { registerX = result
            , status = status'
            }

brk :: AddressingMode -> Cpu -> Cpu
brk _ cpu = cpu

getOperation :: OpCode -> Operation
getOperation BRK          = brk NoneAddressing
getOperation TAX          = tax NoneAddressing
getOperation INX          = inx NoneAddressing
getOperation LDAImmediate = lda Immediate
getOperation LDAZeroPage  = lda ZeroPage
getOperation LDAZeroPageX = lda ZeroPageX
getOperation LDAAbsolute  = lda Absolute
getOperation LDAAbsoluteX = lda AbsoluteX
getOperation LDAAbsoluteY = lda AbsoluteY
getOperation LDAIndirectX = lda IndirectX
getOperation LDAIndirectY = lda IndirectY
getOperation LDXImmediate = ldx Immediate
getOperation LDXZeroPage  = ldx ZeroPage
getOperation LDXZeroPageY = ldx ZeroPageY
getOperation LDXAbsolute  = ldx Absolute
getOperation LDXAbsoluteY = ldx AbsoluteY
getOperation LDYImmediate = ldy Immediate
getOperation LDYZeroPage  = ldy ZeroPage
getOperation LDYZeroPageY = ldy ZeroPageY
getOperation LDYAbsolute  = ldy Absolute
getOperation LDYAbsoluteY = ldy AbsoluteY
getOperation STAZeroPage  = sta ZeroPage
getOperation STAZeroPageX = sta ZeroPageX
getOperation STAAbsolute  = sta Absolute
getOperation STAAbsoluteX = sta AbsoluteX
getOperation STAAbsoluteY = sta AbsoluteY
getOperation STAIndirectX = sta IndirectX
getOperation STAIndirectY = sta IndirectY
getOperation ADCImmediate = adc Immediate
getOperation ADCZeroPage  = adc ZeroPage
getOperation ADCZeroPageX = adc ZeroPageX
getOperation ADCAbsolute  = adc Absolute
getOperation ADCAbsoluteX = adc AbsoluteX
getOperation ADCAbsoluteY = adc AbsoluteY
getOperation ADCIndirectX = adc IndirectX
getOperation ADCIndirectY = adc IndirectY
getOperation SBCImmediate = sbc Immediate
getOperation SBCZeroPage  = sbc ZeroPage
getOperation SBCZeroPageX = sbc ZeroPageX
getOperation SBCAbsolute  = sbc Absolute
getOperation SBCAbsoluteX = sbc AbsoluteX
getOperation SBCAbsoluteY = sbc AbsoluteY
getOperation SBCIndirectX = sbc IndirectX
getOperation SBCIndirectY = sbc IndirectY
getOperation _            = error "Unknown opcode"

