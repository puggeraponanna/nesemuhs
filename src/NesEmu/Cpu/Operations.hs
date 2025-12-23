module NesEmu.Cpu.Operations (getOperation, getOperandLength) where

import           Data.Bits
import           Data.Int           (Int8)
import           Data.Word          (Word16, Word8)
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
    let a = fromIntegral (registerA cpu) :: Word16
        b = fromIntegral (memoryRead cpu (getOperandAddress cpu addrMode)) :: Word16
        c = if getFlag Carry cpu then 1 else 0
        total = a + b + c
        result = fromIntegral (total .&. 0xFF) :: Word8
        status' =
            status $
                setZF result $
                    setNF result $
                        setCF (total > 0xFF) $
                            setVF (fromIntegral a) (fromIntegral b) result cpu
     in cpu
            { registerA = result
            , status = status'
            }

sbc :: AddressingMode -> Cpu -> Cpu
sbc addrMode cpu =
    let a = fromIntegral (registerA cpu) :: Word16
        b = fromIntegral (memoryRead cpu (getOperandAddress cpu addrMode)) :: Word16
        c = if getFlag Carry cpu then 1 else 0
        b' = b `xor` 0xFF
        total = a + b' + c
        result = fromIntegral (total .&. 0xFF) :: Word8
        status' =
            status $
                setZF result $
                    setNF result $
                        setCF (total > 0xFF) $
                            setVF (fromIntegral a) (fromIntegral b') result cpu
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

dex :: AddressingMode -> Cpu -> Cpu
dex _ cpu =
    let result = registerX cpu - 1
        status' = status $ setZF result $ setNF result cpu
     in cpu
            { registerX = result
            , status = status'
            }

dey :: AddressingMode -> Cpu -> Cpu
dey _ cpu =
    let result = registerY cpu - 1
        status' = status $ setZF result $ setNF result cpu
     in cpu
            { registerY = result
            , status = status'
            }

inc :: AddressingMode -> Cpu -> Cpu
inc addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        val = memoryRead cpu addr
        result = val + 1
        cpu' = memoryWrite cpu addr result
     in setZF result $ setNF result cpu'

dec :: AddressingMode -> Cpu -> Cpu
dec addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        val = memoryRead cpu addr
        result = val - 1
        cpu' = memoryWrite cpu addr result
     in setZF result $ setNF result cpu'

brk :: AddressingMode -> Cpu -> Cpu
brk _ cpu = cpu

and_ :: AddressingMode -> Cpu -> Cpu
and_ addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        result = registerA cpu .&. memoryRead cpu addr
        cpu' = setZF result $ setNF result cpu
     in cpu'{registerA = result}

eor :: AddressingMode -> Cpu -> Cpu
eor addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        result = registerA cpu `xor` memoryRead cpu addr
        cpu' = setZF result $ setNF result cpu
     in cpu'{registerA = result}

ora :: AddressingMode -> Cpu -> Cpu
ora addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        result = registerA cpu .|. memoryRead cpu addr
        cpu' = setZF result $ setNF result cpu
     in cpu'{registerA = result}

bit_ :: AddressingMode -> Cpu -> Cpu
bit_ addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        val = memoryRead cpu addr
        res = registerA cpu .&. val
        cpu' = setZF res cpu
        cpu'' = setFlag Negative (testBit val 7) cpu'
        cpu''' = setFlag Overflow (testBit val 6) cpu''
     in cpu'''

compare_ :: Word8 -> AddressingMode -> Cpu -> Cpu
compare_ reg addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        val = memoryRead cpu addr
        res = reg - val
        cpu' = setCF (reg >= val) cpu
        cpu'' = setZF res cpu'
     in setNF res cpu''

cmp :: AddressingMode -> Cpu -> Cpu
cmp addrMode cpu = compare_ (registerA cpu) addrMode cpu

cpx :: AddressingMode -> Cpu -> Cpu
cpx addrMode cpu = compare_ (registerX cpu) addrMode cpu

cpy :: AddressingMode -> Cpu -> Cpu
cpy addrMode cpu = compare_ (registerY cpu) addrMode cpu

asl :: AddressingMode -> Cpu -> Cpu
asl Accumulator cpu =
    let val = registerA cpu
        cpu' = setCF (testBit val 7) cpu
        result = val `shiftL` 1
        cpu'' = setZF result $ setNF result cpu'
     in cpu''{registerA = result}
asl addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        val = memoryRead cpu addr
        cpu' = setCF (testBit val 7) cpu
        result = val `shiftL` 1
        cpu'' = setZF result $ setNF result cpu'
     in memoryWrite cpu'' addr result

lsr :: AddressingMode -> Cpu -> Cpu
lsr Accumulator cpu =
    let val = registerA cpu
        cpu' = setCF (testBit val 0) cpu
        result = val `shiftR` 1
        cpu'' = setZF result $ setNF result cpu'
     in cpu''{registerA = result}
lsr addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        val = memoryRead cpu addr
        cpu' = setCF (testBit val 0) cpu
        result = val `shiftR` 1
        cpu'' = setZF result $ setNF result cpu'
     in memoryWrite cpu'' addr result

rol :: AddressingMode -> Cpu -> Cpu
rol Accumulator cpu =
    let val = registerA cpu
        oldCarry = if getFlag Carry cpu then 1 else 0
        cpu' = setCF (testBit val 7) cpu
        result = (val `shiftL` 1) .|. oldCarry
        cpu'' = setZF result $ setNF result cpu'
     in cpu''{registerA = result}
rol addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        val = memoryRead cpu addr
        oldCarry = if getFlag Carry cpu then 1 else 0
        cpu' = setCF (testBit val 7) cpu
        result = (val `shiftL` 1) .|. oldCarry
        cpu'' = setZF result $ setNF result cpu'
     in memoryWrite cpu'' addr result

ror :: AddressingMode -> Cpu -> Cpu
ror Accumulator cpu =
    let val = registerA cpu
        oldCarry = if getFlag Carry cpu then 0x80 else 0
        cpu' = setCF (testBit val 0) cpu
        result = (val `shiftR` 1) .|. oldCarry
        cpu'' = setZF result $ setNF result cpu'
     in cpu''{registerA = result}
ror addrMode cpu =
    let addr = getOperandAddress cpu addrMode
        val = memoryRead cpu addr
        oldCarry = if getFlag Carry cpu then 0x80 else 0
        cpu' = setCF (testBit val 0) cpu
        result = (val `shiftR` 1) .|. oldCarry
        cpu'' = setZF result $ setNF result cpu'
     in memoryWrite cpu'' addr result

stackPush :: Word8 -> Cpu -> Cpu
stackPush val cpu =
    let addr = 0x0100 + fromIntegral (stackPointer cpu)
        cpu' = memoryWrite cpu addr val
     in cpu'{stackPointer = stackPointer cpu - 1}

stackPop :: Cpu -> (Word8, Cpu)
stackPop cpu =
    let sp' = stackPointer cpu + 1
        addr = 0x0100 + fromIntegral sp'
        val = memoryRead cpu addr
     in (val, cpu{stackPointer = sp'})

stackPush16 :: Word16 -> Cpu -> Cpu
stackPush16 val cpu =
    let hi = fromIntegral (val `shiftR` 8) :: Word8
        lo = fromIntegral (val .&. 0xFF) :: Word8
     in stackPush hi $ stackPush lo cpu

stackPop16 :: Cpu -> (Word16, Cpu)
stackPop16 cpu =
    let (lo, cpu') = stackPop cpu
        (hi, cpu'') = stackPop cpu'
        val = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
     in (val, cpu'')

branch :: Bool -> Cpu -> Cpu
branch condition cpu =
    if condition
        then
            let offset = fromIntegral (memoryRead cpu (programCounter cpu)) :: Int8
                target = (fromIntegral (programCounter cpu) + fromIntegral offset) :: Int
             in cpu{programCounter = fromIntegral target}
        else cpu

bcc :: AddressingMode -> Cpu -> Cpu
bcc _ cpu = branch (not $ getFlag Carry cpu) cpu

bcs :: AddressingMode -> Cpu -> Cpu
bcs _ cpu = branch (getFlag Carry cpu) cpu

beq :: AddressingMode -> Cpu -> Cpu
beq _ cpu = branch (getFlag Zero cpu) cpu

bmi :: AddressingMode -> Cpu -> Cpu
bmi _ cpu = branch (getFlag Negative cpu) cpu

bne :: AddressingMode -> Cpu -> Cpu
bne _ cpu = branch (not $ getFlag Zero cpu) cpu

bpl :: AddressingMode -> Cpu -> Cpu
bpl _ cpu = branch (not $ getFlag Negative cpu) cpu

bvc :: AddressingMode -> Cpu -> Cpu
bvc _ cpu = branch (not $ getFlag Overflow cpu) cpu

bvs :: AddressingMode -> Cpu -> Cpu
bvs _ cpu = branch (getFlag Overflow cpu) cpu

jmp :: AddressingMode -> Cpu -> Cpu
jmp Absolute cpu =
    let addr = getOperandAddress cpu Absolute
     in cpu{programCounter = addr - 2}
jmp Indirect cpu =
    let ptr = getOperandAddress cpu Absolute
        lo = memoryRead cpu ptr
        hi = memoryRead cpu (if (ptr .&. 0xFF) == 0xFF then ptr .&. 0xFF00 else ptr + 1)
        addr = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo
     in cpu{programCounter = addr - 2}
jmp _ _ = error "Invalid JMP addressing mode"

jsr :: AddressingMode -> Cpu -> Cpu
jsr addrMode cpu =
    let target = getOperandAddress cpu addrMode
        cpu' = stackPush16 (programCounter cpu + 1) cpu
     in cpu'{programCounter = target - 2}

rts :: AddressingMode -> Cpu -> Cpu
rts _ cpu =
    let (addr, cpu') = stackPop16 cpu
     in cpu'{programCounter = addr + 1}

rti :: AddressingMode -> Cpu -> Cpu
rti _ cpu =
    let (stat, cpu') = stackPop cpu
        (pc, cpu'') = stackPop16 cpu'
        stat' = (stat .&. 0xEF) .|. 0x20
     in cpu''{status = stat', programCounter = pc}

clc, cld, cli, clv, sec, sed, sei :: AddressingMode -> Cpu -> Cpu
clc _ = setFlag Carry False
cld _ = setFlag Decimal False
cli _ = setFlag Interrupt False
clv _ = setFlag Overflow False
sec _ = setFlag Carry True
sed _ = setFlag Decimal True
sei _ = setFlag Interrupt True

pha :: AddressingMode -> Cpu -> Cpu
pha _ cpu = stackPush (registerA cpu) cpu

php :: AddressingMode -> Cpu -> Cpu
php _ cpu = stackPush (status cpu .|. 0x30) cpu

pla :: AddressingMode -> Cpu -> Cpu
pla _ cpu =
    let (val, cpu') = stackPop cpu
        cpu'' = setZF val $ setNF val cpu'
     in cpu''{registerA = val}

plp :: AddressingMode -> Cpu -> Cpu
plp _ cpu =
    let (val, cpu') = stackPop cpu
     in cpu'{status = (val .&. 0xEF) .|. 0x20}

tay :: AddressingMode -> Cpu -> Cpu
tay _ cpu =
    let val = registerA cpu
     in setZF val $ setNF val $ cpu{registerY = val}

tya :: AddressingMode -> Cpu -> Cpu
tya _ cpu =
    let val = registerY cpu
     in setZF val $ setNF val $ cpu{registerA = val}

txa :: AddressingMode -> Cpu -> Cpu
txa _ cpu =
    let val = registerX cpu
     in setZF val $ setNF val $ cpu{registerA = val}

tsx :: AddressingMode -> Cpu -> Cpu
tsx _ cpu =
    let val = stackPointer cpu
     in setZF val $ setNF val $ cpu{registerX = val}

txs :: AddressingMode -> Cpu -> Cpu
txs _ cpu = cpu{stackPointer = registerX cpu}

nop :: AddressingMode -> Cpu -> Cpu
nop _ cpu = cpu

getOperation :: OpCode -> Operation
getOperation BRK            = brk NoneAddressing
getOperation TAX            = tax NoneAddressing
getOperation INX            = inx NoneAddressing
getOperation DEX            = dex NoneAddressing
getOperation DEY            = dey NoneAddressing
getOperation INCZeroPage    = inc ZeroPage
getOperation INCZeroPageX   = inc ZeroPageX
getOperation INCAbsolute    = inc Absolute
getOperation INCAbsoluteX   = inc AbsoluteX
getOperation DECZeroPage    = dec ZeroPage
getOperation DECZeroPageX   = dec ZeroPageX
getOperation DECAbsolute    = dec Absolute
getOperation DECAbsoluteX   = dec AbsoluteX
getOperation LDAImmediate   = lda Immediate
getOperation LDAZeroPage    = lda ZeroPage
getOperation LDAZeroPageX   = lda ZeroPageX
getOperation LDAAbsolute    = lda Absolute
getOperation LDAAbsoluteX   = lda AbsoluteX
getOperation LDAAbsoluteY   = lda AbsoluteY
getOperation LDAIndirectX   = lda IndirectX
getOperation LDAIndirectY   = lda IndirectY
getOperation LDXImmediate   = ldx Immediate
getOperation LDXZeroPage    = ldx ZeroPage
getOperation LDXZeroPageY   = ldx ZeroPageY
getOperation LDXAbsolute    = ldx Absolute
getOperation LDXAbsoluteY   = ldx AbsoluteY
getOperation LDYImmediate   = ldy Immediate
getOperation LDYZeroPage    = ldy ZeroPage
getOperation LDYZeroPageY   = ldy ZeroPageY
getOperation LDYAbsolute    = ldy Absolute
getOperation LDYAbsoluteY   = ldy AbsoluteY
getOperation STAZeroPage    = sta ZeroPage
getOperation STAZeroPageX   = sta ZeroPageX
getOperation STAAbsolute    = sta Absolute
getOperation STAAbsoluteX   = sta AbsoluteX
getOperation STAAbsoluteY   = sta AbsoluteY
getOperation STAIndirectX   = sta IndirectX
getOperation STAIndirectY   = sta IndirectY
getOperation ADCImmediate   = adc Immediate
getOperation ADCZeroPage    = adc ZeroPage
getOperation ADCZeroPageX   = adc ZeroPageX
getOperation ADCAbsolute    = adc Absolute
getOperation ADCAbsoluteX   = adc AbsoluteX
getOperation ADCAbsoluteY   = adc AbsoluteY
getOperation ADCIndirectX   = adc IndirectX
getOperation ADCIndirectY   = adc IndirectY
getOperation SBCImmediate   = sbc Immediate
getOperation SBCZeroPage    = sbc ZeroPage
getOperation SBCZeroPageX   = sbc ZeroPageX
getOperation SBCAbsolute    = sbc Absolute
getOperation SBCAbsoluteX   = sbc AbsoluteX
getOperation SBCAbsoluteY   = sbc AbsoluteY
getOperation SBCIndirectX   = sbc IndirectX
getOperation SBCIndirectY   = sbc IndirectY
getOperation ANDImmediate   = and_ Immediate
getOperation ANDZeroPage    = and_ ZeroPage
getOperation ANDZeroPageX   = and_ ZeroPageX
getOperation ANDAbsolute    = and_ Absolute
getOperation ANDAbsoluteX   = and_ AbsoluteX
getOperation ANDAbsoluteY   = and_ AbsoluteY
getOperation ANDIndirectX   = and_ IndirectX
getOperation ANDIndirectY   = and_ IndirectY
getOperation EORImmediate   = eor Immediate
getOperation EORZeroPage    = eor ZeroPage
getOperation EORZeroPageX   = eor ZeroPageX
getOperation EORAbsolute    = eor Absolute
getOperation EORAbsoluteX   = eor AbsoluteX
getOperation EORAbsoluteY   = eor AbsoluteY
getOperation EORIndirectX   = eor IndirectX
getOperation EORIndirectY   = eor IndirectY
getOperation ORAImmediate   = ora Immediate
getOperation ORAZeroPage    = ora ZeroPage
getOperation ORAZeroPageX   = ora ZeroPageX
getOperation ORAAbsolute    = ora Absolute
getOperation ORAAbsoluteX   = ora AbsoluteX
getOperation ORAAbsoluteY   = ora AbsoluteY
getOperation ORAIndirectX   = ora IndirectX
getOperation ORAIndirectY   = ora IndirectY
getOperation BITZeroPage    = bit_ ZeroPage
getOperation BITAbsolute    = bit_ Absolute
getOperation CMPImmediate   = cmp Immediate
getOperation CMPZeroPage    = cmp ZeroPage
getOperation CMPZeroPageX   = cmp ZeroPageX
getOperation CMPAbsolute    = cmp Absolute
getOperation CMPAbsoluteX   = cmp AbsoluteX
getOperation CMPAbsoluteY   = cmp AbsoluteY
getOperation CMPIndirectX   = cmp IndirectX
getOperation CMPIndirectY   = cmp IndirectY
getOperation CPXImmediate   = cpx Immediate
getOperation CPXZeroPage    = cpx ZeroPage
getOperation CPXAbsolute    = cpx Absolute
getOperation CPYImmediate   = cpy Immediate
getOperation CPYZeroPage    = cpy ZeroPage
getOperation CPYAbsolute    = cpy Absolute
getOperation ASLAccumulator = asl Accumulator
getOperation ASLZeroPage    = asl ZeroPage
getOperation ASLZeroPageX   = asl ZeroPageX
getOperation ASLAbsolute    = asl Absolute
getOperation ASLAbsoluteX   = asl AbsoluteX
getOperation LSRAccumulator = lsr Accumulator
getOperation LSRZeroPage    = lsr ZeroPage
getOperation LSRZeroPageX   = lsr ZeroPageX
getOperation LSRAbsolute    = lsr Absolute
getOperation LSRAbsoluteX   = lsr AbsoluteX
getOperation ROLAccumulator = rol Accumulator
getOperation ROLZeroPage    = rol ZeroPage
getOperation ROLZeroPageX   = rol ZeroPageX
getOperation ROLAbsolute    = rol Absolute
getOperation ROLAbsoluteX   = rol AbsoluteX
getOperation RORAccumulator = ror Accumulator
getOperation RORZeroPage    = ror ZeroPage
getOperation RORZeroPageX   = ror ZeroPageX
getOperation RORAbsolute    = ror Absolute
getOperation RORAbsoluteX   = ror AbsoluteX
getOperation BCC            = bcc Relative
getOperation BCS            = bcs Relative
getOperation BEQ            = beq Relative
getOperation BMI            = bmi Relative
getOperation BNE            = bne Relative
getOperation BPL            = bpl Relative
getOperation BVC            = bvc Relative
getOperation BVS            = bvs Relative
getOperation JMPAbsolute    = jmp Absolute
getOperation JMPIndirect    = jmp Indirect
getOperation JSRAbsolute    = jsr Absolute
getOperation RTS            = rts NoneAddressing
getOperation RTI            = rti NoneAddressing
getOperation CLC            = clc NoneAddressing
getOperation CLD            = cld NoneAddressing
getOperation CLI            = cli NoneAddressing
getOperation CLV            = clv NoneAddressing
getOperation SEC            = sec NoneAddressing
getOperation SED            = sed NoneAddressing
getOperation SEI            = sei NoneAddressing
getOperation PHA            = pha NoneAddressing
getOperation PHP            = php NoneAddressing
getOperation PLA            = pla NoneAddressing
getOperation PLP            = plp NoneAddressing
getOperation TAY            = tay NoneAddressing
getOperation TYA            = tya NoneAddressing
getOperation TSX            = tsx NoneAddressing
getOperation TXS            = txs NoneAddressing
getOperation TXA            = txa NoneAddressing
getOperation NOP            = nop NoneAddressing
getOperation _              = error "Unknown opcode"
