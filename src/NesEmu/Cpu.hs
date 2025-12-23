module NesEmu.Cpu where

import           Data.Word
import           NesEmu.Cpu.Memory
import           NesEmu.Cpu.Opcodes
import           NesEmu.Cpu.Operations
import           NesEmu.Cpu.Types

reset :: Cpu -> Cpu
reset cpu =
    cpu
        { registerA = 0
        , registerX = 0
        , registerY = 0
        , status = 0
        , programCounter = memoryRead16 cpu 0xFFFC
        }

newCpu :: Cpu
newCpu =
    Cpu
        { registerA = 0
        , registerX = 0
        , registerY = 0
        , status = 0
        , programCounter = 0
        , memory = replicate 0x10000 0
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
    let opcode = memoryRead cpu (programCounter cpu)
        cpu' = incrementPc cpu 1
        cpu'' = getOperation opcode cpu'
        cpu''' = incrementPc cpu'' $ getOperandLength opcode
     in case opcode of
            BRK -> cpu'''
            _   -> run cpu'''

incrementPc :: Cpu -> Word16 -> Cpu
incrementPc cpu i = cpu{programCounter = programCounter cpu + i}
