module NesEmu.Cpu.Opcodes where

import           Data.Word (Word16, Word8)

type OpCode = Word8

pattern BRK :: OpCode
pattern BRK = 0x00

pattern TAX :: OpCode
pattern TAX = 0xAA

pattern INX :: OpCode
pattern INX = 0xE8

pattern LDAImmediate :: OpCode
pattern LDAImmediate = 0xA9

pattern LDAZeroPage :: OpCode
pattern LDAZeroPage = 0xA5

pattern LDAZeroPageX :: OpCode
pattern LDAZeroPageX = 0xB5

pattern LDAAbsolute :: OpCode
pattern LDAAbsolute = 0xAD

pattern LDAAbsoluteX :: OpCode
pattern LDAAbsoluteX = 0xBD

pattern LDAAbsoluteY :: OpCode
pattern LDAAbsoluteY = 0xB9

pattern LDAIndirectX :: OpCode
pattern LDAIndirectX = 0xA1

pattern LDAIndirectY :: OpCode
pattern LDAIndirectY = 0xB1

pattern LDXImmediate :: OpCode
pattern LDXImmediate = 0xA2

pattern LDXZeroPage :: OpCode
pattern LDXZeroPage = 0xA6

pattern LDXZeroPageY :: OpCode
pattern LDXZeroPageY = 0xB6

pattern LDXAbsolute :: OpCode
pattern LDXAbsolute = 0xAE

pattern LDXAbsoluteY :: OpCode
pattern LDXAbsoluteY = 0xBE

pattern LDYImmediate :: OpCode
pattern LDYImmediate = 0xA0

pattern LDYZeroPage :: OpCode
pattern LDYZeroPage = 0xA4

pattern LDYZeroPageY :: OpCode
pattern LDYZeroPageY = 0xB4

pattern LDYAbsolute :: OpCode
pattern LDYAbsolute = 0xAC

pattern LDYAbsoluteY :: OpCode
pattern LDYAbsoluteY = 0xBC

pattern STAZeroPage :: OpCode
pattern STAZeroPage = 0x85

pattern STAZeroPageX :: OpCode
pattern STAZeroPageX = 0x95

pattern STAAbsolute :: OpCode
pattern STAAbsolute = 0x8D

pattern STAAbsoluteX :: OpCode
pattern STAAbsoluteX = 0x9D

pattern STAAbsoluteY :: OpCode
pattern STAAbsoluteY = 0x99

pattern STAIndirectX :: OpCode
pattern STAIndirectX = 0x81

pattern STAIndirectY :: OpCode
pattern STAIndirectY = 0x91

pattern ADCImmediate :: OpCode
pattern ADCImmediate = 0x69

pattern ADCZeroPage :: OpCode
pattern ADCZeroPage = 0x65

pattern ADCZeroPageX :: OpCode
pattern ADCZeroPageX = 0x75

pattern ADCAbsolute :: OpCode
pattern ADCAbsolute = 0x6D

pattern ADCAbsoluteX :: OpCode
pattern ADCAbsoluteX = 0x7D

pattern ADCAbsoluteY :: OpCode
pattern ADCAbsoluteY = 0x79

pattern ADCIndirectX :: OpCode
pattern ADCIndirectX = 0x61

pattern ADCIndirectY :: OpCode
pattern ADCIndirectY = 0x71

pattern SBCImmediate :: OpCode
pattern SBCImmediate = 0xE9

pattern SBCZeroPage :: OpCode
pattern SBCZeroPage = 0xE5

pattern SBCZeroPageX :: OpCode
pattern SBCZeroPageX = 0xF5

pattern SBCAbsolute :: OpCode
pattern SBCAbsolute = 0xED

pattern SBCAbsoluteX :: OpCode
pattern SBCAbsoluteX = 0xFD

pattern SBCAbsoluteY :: OpCode
pattern SBCAbsoluteY = 0xF9

pattern SBCIndirectX :: OpCode
pattern SBCIndirectX = 0xE1

pattern SBCIndirectY :: OpCode
pattern SBCIndirectY = 0xF1

pattern DECZeroPage :: OpCode
pattern DECZeroPage = 0xC6

pattern DECZeroPageX :: OpCode
pattern DECZeroPageX = 0xD6

pattern DECAbsolute :: OpCode
pattern DECAbsolute = 0xCE

pattern DECAbsoluteX :: OpCode
pattern DECAbsoluteX = 0xDE

pattern INCZeroPage :: OpCode
pattern INCZeroPage = 0xE6

pattern INCZeroPageX :: OpCode
pattern INCZeroPageX = 0xF6

pattern INCAbsolute :: OpCode
pattern INCAbsolute = 0xEE

pattern INCAbsoluteX :: OpCode
pattern INCAbsoluteX = 0xFE

pattern DEY :: OpCode
pattern DEY = 0x88

pattern DEX :: OpCode
pattern DEX = 0xCA

getOperandLength :: OpCode -> Word16
getOperandLength BRK          = 0
getOperandLength TAX          = 0
getOperandLength INX          = 0
getOperandLength LDAImmediate = 1
getOperandLength LDAZeroPage  = 1
getOperandLength LDAZeroPageX = 1
getOperandLength LDAAbsolute  = 2
getOperandLength LDAAbsoluteX = 2
getOperandLength LDAAbsoluteY = 2
getOperandLength LDAIndirectX = 1
getOperandLength LDAIndirectY = 1
getOperandLength LDXImmediate = 1
getOperandLength LDXZeroPage  = 1
getOperandLength LDXZeroPageY = 1
getOperandLength LDXAbsolute  = 2
getOperandLength LDXAbsoluteY = 2
getOperandLength LDYImmediate = 1
getOperandLength STAZeroPage  = 1
getOperandLength STAZeroPageX = 1
getOperandLength STAAbsolute  = 2
getOperandLength STAAbsoluteX = 2
getOperandLength STAAbsoluteY = 2
getOperandLength STAIndirectX = 1
getOperandLength STAIndirectY = 1
getOperandLength ADCImmediate = 1
getOperandLength ADCZeroPage  = 1
getOperandLength ADCZeroPageX = 1
getOperandLength ADCAbsolute  = 2
getOperandLength ADCAbsoluteX = 2
getOperandLength ADCAbsoluteY = 2
getOperandLength ADCIndirectX = 1
getOperandLength ADCIndirectY = 1
getOperandLength SBCImmediate = 1
getOperandLength SBCZeroPage  = 1
getOperandLength SBCZeroPageX = 1
getOperandLength SBCAbsolute  = 2
getOperandLength SBCAbsoluteX = 2
getOperandLength SBCAbsoluteY = 2
getOperandLength SBCIndirectX = 1
getOperandLength SBCIndirectY = 1
getOperandLength DEX          = 0
getOperandLength DEY          = 0
getOperandLength INCZeroPage  = 1
getOperandLength INCZeroPageX = 1
getOperandLength INCAbsolute  = 2
getOperandLength INCAbsoluteX = 2
getOperandLength DECZeroPage  = 1
getOperandLength DECZeroPageX = 1
getOperandLength DECAbsolute  = 2
getOperandLength DECAbsoluteX = 2
getOperandLength _            = error "Unknown opcode"
