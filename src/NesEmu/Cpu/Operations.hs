module NesEmu.Cpu.Operations where

import           NesEmu.Cpu.Memory
import           NesEmu.Cpu.Types
import           NesEmu.Cpu.Flags

lda :: Cpu -> AddressingMode -> Cpu
lda cpu addrMode =
  let addr = getOperandAddress cpu addrMode
      result = memoryRead cpu addr
      status' = status $ setZF result $ setNF result cpu
   in cpu
        { registerA = result,
          status = status'
        }

sta :: Cpu -> AddressingMode -> Cpu
sta cpu addrMode =
  let addr = getOperandAddress cpu addrMode
      val = registerA cpu
   in memoryWrite cpu addr val

tax :: Cpu -> Cpu
tax cpu =
    let result = registerA cpu
        status' = status $ setZF result $ setNF result  cpu
    in cpu {
         registerX = result,
        status = status'
    }
    

adc :: Cpu -> AddressingMode -> Cpu
adc cpu addrMode =
  let 
    a = registerA cpu
    b = memoryRead cpu (getOperandAddress cpu addrMode)
    carry = if getFlag Carry cpu then 1 else 0
    result = a + b + carry
    status' = status $ setZF result  $ setNF result $ setCF (result > 255) $ setVF a b cpu
  in
    cpu {
      registerA = result,
      status = status'
    }

sbc :: Cpu -> AddressingMode -> Cpu
sbc cpu addrMode =
  let 
    a = registerA cpu
    b = memoryRead cpu (getOperandAddress cpu addrMode)
    carry = if getFlag Carry cpu then 1 else 0
    result = a - b - carry
    status' = status $ setZF result  $ setNF result $ setCF (result > 255) $ setVF a b cpu
  in
    cpu {
      registerA = result,
      status = status'
    }

inx :: Cpu -> Cpu
inx cpu =
    let result = registerX cpu + 1
        status' = status $ setZF result $ setNF result $ setVF result 1 cpu
    in
  cpu
    { registerX = result,
      status = status'
    }
    

brk :: Cpu -> Cpu
brk cpu = cpu
