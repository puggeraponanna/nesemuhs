module NesEmu.Cpu.Opcodes where

import           Data.Word (Word8)


type OpCode = Word8

opTax :: OpCode
opTax = 0xAA

opInx :: OpCode
opInx = 0xE8

opBrk :: OpCode
opBrk = 0x00

opLdaImmediate :: OpCode
opLdaImmediate = 0xA9

opLdaZeroPage :: OpCode
opLdaZeroPage = 0xA5

opLdaZeroPageX :: OpCode
opLdaZeroPageX = 0xB5

opLdaAbsolute :: OpCode
opLdaAbsolute = 0xAD

opLdaAbsoluteX :: OpCode
opLdaAbsoluteX = 0xBD

opLdaAbsoluteY :: OpCode
opLdaAbsoluteY = 0xB9

opLdaIndirectX :: OpCode
opLdaIndirectX = 0xA1

opLdaIndirectY :: OpCode
opLdaIndirectY = 0xB1

opStaZeroPage :: OpCode
opStaZeroPage = 0x85

opStaZeroPageX :: OpCode
opStaZeroPageX = 0x95

opStaAbsolute :: OpCode
opStaAbsolute = 0x8D

opStaAbsoluteX :: OpCode
opStaAbsoluteX = 0x9D

opStaAbsoluteY :: OpCode
opStaAbsoluteY = 0x99

opStaIndirectX :: OpCode
opStaIndirectX = 0x81

opStaIndirectY :: OpCode
opStaIndirectY = 0x91

opAdcImmediate :: OpCode
opAdcImmediate = 0x69

opAdcZeroPage :: OpCode
opAdcZeroPage = 0x65

opAdcZeroPageX :: OpCode
opAdcZeroPageX = 0x75

opAdcAbsolute :: OpCode
opAdcAbsolute = 0x6D

opAdcAbsoluteX :: OpCode
opAdcAbsoluteX = 0x7D

opAdcAbsoluteY :: OpCode
opAdcAbsoluteY = 0x79

opAdcIndirectX :: OpCode
opAdcIndirectX = 0x61

opAdcIndirectY :: OpCode
opAdcIndirectY = 0x71

opSbcImmediate :: OpCode
opSbcImmediate = 0xE9

opSbcZeroPage :: OpCode
opSbcZeroPage = 0xE5

opSbcZeroPageX :: OpCode
opSbcZeroPageX = 0xF5

opSbcAbsolute :: OpCode
opSbcAbsolute = 0xED

opSbcAbsoluteX :: OpCode
opSbcAbsoluteX = 0xFD

opSbcAbsoluteY :: OpCode
opSbcAbsoluteY = 0xF9

opSbcIndirectX :: OpCode
opSbcIndirectX = 0xE1

opSbcIndirectY :: OpCode
opSbcIndirectY = 0xF1
