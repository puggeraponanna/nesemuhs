module NesEmu.Cpu where

import           Data.Word
import           NesEmu.Cpu.Memory
import           NesEmu.Cpu.Opcodes
import           NesEmu.Cpu.Operations
import           NesEmu.Cpu.Types
import           Text.Printf           (printf)

import qualified Data.ByteString       as BS
import           NesEmu.Rom
import           System.IO             (hFlush, stdout)

reset :: Cpu -> Cpu
reset cpu =
    cpu
        { registerA = 0
        , registerX = 0
        , registerY = 0
        , status = 0x24
        , stackPointer = 0xFD
        , programCounter = memoryRead16 cpu 0xFFFC
        }

newCpu :: Cpu
newCpu =
    Cpu
        { registerA = 0
        , registerX = 0
        , registerY = 0
        , status = 0
        , stackPointer = 0xFD
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

loadRomIntoCpu :: Cpu -> NesRom -> Maybe Word16 -> Cpu
loadRomIntoCpu cpu rom pcOverride =
    let prg = BS.unpack (prgRom rom)
        mem = memory cpu
        newMem =
            if length prg == 16384
                then take 0x8000 mem ++ prg ++ prg
                else take 0x8000 mem ++ prg ++ drop (0x8000 + length prg) mem
        cpu' = cpu{memory = newMem}
        cpu'' = reset cpu'
     in case pcOverride of
            Just pc -> cpu''{programCounter = pc}
            Nothing -> cpu''

step :: Cpu -> (Word8, Cpu)
step cpu =
    let opcode = memoryRead cpu (programCounter cpu)
        cpu' = incrementPc cpu 1
        cpu'' = getOperation opcode cpu'
        cpu''' = incrementPc cpu'' $ getOperandLength opcode
     in (opcode, cpu''')

run :: Cpu -> Cpu
run cpu =
    let (opcode, cpu') = step cpu
     in if opcode == 0x00 -- BRK
            then cpu'
            else run cpu'

runDebug :: Cpu -> IO Cpu
runDebug cpu = do
    let opcode = memoryRead cpu (programCounter cpu)
    let logMsg =
            printf
                "%04X  %02X  A:%02X X:%02X Y:%02X P:%02X SP:%02X"
                (programCounter cpu)
                opcode
                (registerA cpu)
                (registerX cpu)
                (registerY cpu)
                (status cpu)
                (stackPointer cpu)
    if opcode == 0x00 -- BRK
        then return cpu
        else do
            putStrLn logMsg
            hFlush stdout
            let (_, cpu') = step cpu
            runDebug cpu'

incrementPc :: Cpu -> Word16 -> Cpu
incrementPc cpu i = cpu{programCounter = programCounter cpu + i}
