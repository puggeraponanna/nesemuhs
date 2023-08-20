module NesEmu.Cpu.Opcodes where

import           Data.Word (Word8)

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
