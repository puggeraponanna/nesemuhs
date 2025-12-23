module NesEmu.Cpu.Opcodes where

import           Data.Word (Word16, Word8)

type OpCode = Word8

pattern BRK, TAX, INX, INY, DEX, DEY, NOP :: OpCode
pattern BRK = 0x00
pattern TAX = 0xAA
pattern INX = 0xE8
pattern INY = 0xC8
pattern DEX = 0xCA
pattern DEY = 0x88
pattern NOP = 0xEA

-- LDA
pattern LDAImmediate, LDAZeroPage, LDAZeroPageX, LDAAbsolute, LDAAbsoluteX, LDAAbsoluteY, LDAIndirectX, LDAIndirectY :: OpCode
pattern LDAImmediate = 0xA9
pattern LDAZeroPage = 0xA5
pattern LDAZeroPageX = 0xB5
pattern LDAAbsolute = 0xAD
pattern LDAAbsoluteX = 0xBD
pattern LDAAbsoluteY = 0xB9
pattern LDAIndirectX = 0xA1
pattern LDAIndirectY = 0xB1

-- LDX
pattern LDXImmediate, LDXZeroPage, LDXZeroPageY, LDXAbsolute, LDXAbsoluteY :: OpCode
pattern LDXImmediate = 0xA2
pattern LDXZeroPage = 0xA6
pattern LDXZeroPageY = 0xB6
pattern LDXAbsolute = 0xAE
pattern LDXAbsoluteY = 0xBE

-- LDY
pattern LDYImmediate, LDYZeroPage, LDYZeroPageX, LDYAbsolute, LDYAbsoluteX :: OpCode
pattern LDYImmediate = 0xA0
pattern LDYZeroPage = 0xA4
pattern LDYZeroPageX = 0xB4
pattern LDYAbsolute = 0xAC
pattern LDYAbsoluteX = 0xBC

-- STA
pattern STAZeroPage, STAZeroPageX, STAAbsolute, STAAbsoluteX, STAAbsoluteY, STAIndirectX, STAIndirectY :: OpCode
pattern STAZeroPage = 0x85
pattern STAZeroPageX = 0x95
pattern STAAbsolute = 0x8D
pattern STAAbsoluteX = 0x9D
pattern STAAbsoluteY = 0x99
pattern STAIndirectX = 0x81
pattern STAIndirectY = 0x91

-- STX
pattern STXZeroPage, STXZeroPageY, STXAbsolute :: OpCode
pattern STXZeroPage = 0x86
pattern STXZeroPageY = 0x96
pattern STXAbsolute = 0x8E

-- STY
pattern STYZeroPage, STYZeroPageX, STYAbsolute :: OpCode
pattern STYZeroPage = 0x84
pattern STYZeroPageX = 0x94
pattern STYAbsolute = 0x8C

-- ADC
pattern ADCImmediate, ADCZeroPage, ADCZeroPageX, ADCAbsolute, ADCAbsoluteX, ADCAbsoluteY, ADCIndirectX, ADCIndirectY :: OpCode
pattern ADCImmediate = 0x69
pattern ADCZeroPage = 0x65
pattern ADCZeroPageX = 0x75
pattern ADCAbsolute = 0x6D
pattern ADCAbsoluteX = 0x7D
pattern ADCAbsoluteY = 0x79
pattern ADCIndirectX = 0x61
pattern ADCIndirectY = 0x71

-- SBC
pattern SBCImmediate, SBCZeroPage, SBCZeroPageX, SBCAbsolute, SBCAbsoluteX, SBCAbsoluteY, SBCIndirectX, SBCIndirectY :: OpCode
pattern SBCImmediate = 0xE9
pattern SBCZeroPage = 0xE5
pattern SBCZeroPageX = 0xF5
pattern SBCAbsolute = 0xED
pattern SBCAbsoluteX = 0xFD
pattern SBCAbsoluteY = 0xF9
pattern SBCIndirectX = 0xE1
pattern SBCIndirectY = 0xF1

-- INC/DEC
pattern INCZeroPage, INCZeroPageX, INCAbsolute, INCAbsoluteX :: OpCode
pattern INCZeroPage = 0xE6
pattern INCZeroPageX = 0xF6
pattern INCAbsolute = 0xEE
pattern INCAbsoluteX = 0xFE

pattern DECZeroPage, DECZeroPageX, DECAbsolute, DECAbsoluteX :: OpCode
pattern DECZeroPage = 0xC6
pattern DECZeroPageX = 0xD6
pattern DECAbsolute = 0xCE
pattern DECAbsoluteX = 0xDE

-- Bitwise
pattern ANDImmediate, ANDZeroPage, ANDZeroPageX, ANDAbsolute, ANDAbsoluteX, ANDAbsoluteY, ANDIndirectX, ANDIndirectY :: OpCode
pattern ANDImmediate = 0x29
pattern ANDZeroPage = 0x25
pattern ANDZeroPageX = 0x35
pattern ANDAbsolute = 0x2D
pattern ANDAbsoluteX = 0x3D
pattern ANDAbsoluteY = 0x39
pattern ANDIndirectX = 0x21
pattern ANDIndirectY = 0x31

pattern EORImmediate, EORZeroPage, EORZeroPageX, EORAbsolute, EORAbsoluteX, EORAbsoluteY, EORIndirectX, EORIndirectY :: OpCode
pattern EORImmediate = 0x49
pattern EORZeroPage = 0x45
pattern EORZeroPageX = 0x55
pattern EORAbsolute = 0x4D
pattern EORAbsoluteX = 0x5D
pattern EORAbsoluteY = 0x59
pattern EORIndirectX = 0x41
pattern EORIndirectY = 0x51

pattern ORAImmediate, ORAZeroPage, ORAZeroPageX, ORAAbsolute, ORAAbsoluteX, ORAAbsoluteY, ORAIndirectX, ORAIndirectY :: OpCode
pattern ORAImmediate = 0x09
pattern ORAZeroPage = 0x05
pattern ORAZeroPageX = 0x15
pattern ORAAbsolute = 0x0D
pattern ORAAbsoluteX = 0x1D
pattern ORAAbsoluteY = 0x19
pattern ORAIndirectX = 0x01
pattern ORAIndirectY = 0x11

pattern BITZeroPage, BITAbsolute :: OpCode
pattern BITZeroPage = 0x24
pattern BITAbsolute = 0x2C

-- Compare
pattern CMPImmediate, CMPZeroPage, CMPZeroPageX, CMPAbsolute, CMPAbsoluteX, CMPAbsoluteY, CMPIndirectX, CMPIndirectY :: OpCode
pattern CMPImmediate = 0xC9
pattern CMPZeroPage = 0xC5
pattern CMPZeroPageX = 0xD5
pattern CMPAbsolute = 0xCD
pattern CMPAbsoluteX = 0xDD
pattern CMPAbsoluteY = 0xD9
pattern CMPIndirectX = 0xC1
pattern CMPIndirectY = 0xD1

pattern CPXImmediate, CPXZeroPage, CPXAbsolute :: OpCode
pattern CPXImmediate = 0xE0
pattern CPXZeroPage = 0xE4
pattern CPXAbsolute = 0xEC

pattern CPYImmediate, CPYZeroPage, CPYAbsolute :: OpCode
pattern CPYImmediate = 0xC0
pattern CPYZeroPage = 0xC4
pattern CPYAbsolute = 0xCC

-- Shifts
pattern ASLAccumulator, ASLZeroPage, ASLZeroPageX, ASLAbsolute, ASLAbsoluteX :: OpCode
pattern ASLAccumulator = 0x0A
pattern ASLZeroPage = 0x06
pattern ASLZeroPageX = 0x16
pattern ASLAbsolute = 0x0E
pattern ASLAbsoluteX = 0x1E

pattern LSRAccumulator, LSRZeroPage, LSRZeroPageX, LSRAbsolute, LSRAbsoluteX :: OpCode
pattern LSRAccumulator = 0x4A
pattern LSRZeroPage = 0x46
pattern LSRZeroPageX = 0x56
pattern LSRAbsolute = 0x4E
pattern LSRAbsoluteX = 0x5E

pattern ROLAccumulator, ROLZeroPage, ROLZeroPageX, ROLAbsolute, ROLAbsoluteX :: OpCode
pattern ROLAccumulator = 0x2A
pattern ROLZeroPage = 0x26
pattern ROLZeroPageX = 0x36
pattern ROLAbsolute = 0x2E
pattern ROLAbsoluteX = 0x3E

pattern RORAccumulator, RORZeroPage, RORZeroPageX, RORAbsolute, RORAbsoluteX :: OpCode
pattern RORAccumulator = 0x6A
pattern RORZeroPage = 0x66
pattern RORZeroPageX = 0x76
pattern RORAbsolute = 0x6E
pattern RORAbsoluteX = 0x7E

-- Branch
pattern BCC, BCS, BEQ, BMI, BNE, BPL, BVC, BVS :: OpCode
pattern BCC = 0x90
pattern BCS = 0xB0
pattern BEQ = 0xF0
pattern BMI = 0x30
pattern BNE = 0xD0
pattern BPL = 0x10
pattern BVC = 0x50
pattern BVS = 0x70

-- Jump
pattern JMPAbsolute, JMPIndirect, JSRAbsolute, RTS, RTI :: OpCode
pattern JMPAbsolute = 0x4C
pattern JMPIndirect = 0x6C
pattern JSRAbsolute = 0x20
pattern RTS = 0x60
pattern RTI = 0x40

-- Flags
pattern CLC, CLD, CLI, CLV, SEC, SED, SEI :: OpCode
pattern CLC = 0x18
pattern CLD = 0xD8
pattern CLI = 0x58
pattern CLV = 0xB8
pattern SEC = 0x38
pattern SED = 0xF8
pattern SEI = 0x78

-- Stack
pattern PHA, PHP, PLA, PLP :: OpCode
pattern PHA = 0x48
pattern PHP = 0x08
pattern PLA = 0x68
pattern PLP = 0x28

-- Transfers
pattern TAY, TYA, TSX, TXS, TXA :: OpCode
pattern TAY = 0xA8
pattern TYA = 0x98
pattern TSX = 0xBA
pattern TXS = 0x9A
pattern TXA = 0x8A

-- --- ILLEGAL OPCODES ---

-- Illegal NOPs (1-byte)
pattern NOP_1A, NOP_3A, NOP_5A, NOP_7A, NOP_DA, NOP_FA :: OpCode
pattern NOP_1A = 0x1A
pattern NOP_3A = 0x3A
pattern NOP_5A = 0x5A
pattern NOP_7A = 0x7A
pattern NOP_DA = 0xDA
pattern NOP_FA = 0xFA

-- Illegal NOPs (2-byte)
-- 0x04, 0x14, 0x34, 0x44, 0x54, 0x64, 0x74, 0x80, 0x82, 0x89, 0xC2, 0xE2, 0xD4, 0xF4
pattern NOP_04, NOP_14, NOP_34, NOP_44, NOP_54, NOP_64, NOP_74, NOP_80, NOP_82, NOP_89, NOP_C2, NOP_E2, NOP_D4, NOP_F4 :: OpCode
pattern NOP_04 = 0x04
pattern NOP_14 = 0x14
pattern NOP_34 = 0x34
pattern NOP_44 = 0x44
pattern NOP_54 = 0x54
pattern NOP_64 = 0x64
pattern NOP_74 = 0x74
pattern NOP_80 = 0x80
pattern NOP_82 = 0x82
pattern NOP_89 = 0x89
pattern NOP_C2 = 0xC2
pattern NOP_E2 = 0xE2
pattern NOP_D4 = 0xD4
pattern NOP_F4 = 0xF4

-- Illegal NOPs (3-byte)
-- 0x0C, 0x1C, 0x3C, 0x5C, 0x7C
pattern NOP_0C, NOP_1C, NOP_3C, NOP_5C, NOP_7C, NOP_DC, NOP_FC :: OpCode
pattern NOP_0C = 0x0C
pattern NOP_1C = 0x1C
pattern NOP_3C = 0x3C
pattern NOP_5C = 0x5C
pattern NOP_7C = 0x7C
pattern NOP_DC = 0xDC
pattern NOP_FC = 0xFC

pattern SBC_EB :: OpCode
pattern SBC_EB = 0xEB

-- Illegal LAX
pattern LAXZeroPage, LAXZeroPageY, LAXAbsolute, LAXAbsoluteY, LAXIndirectX, LAXIndirectY :: OpCode
pattern LAXZeroPage = 0xA7
pattern LAXZeroPageY = 0xB7
pattern LAXAbsolute = 0xAF
pattern LAXAbsoluteY = 0xBF
pattern LAXIndirectX = 0xA3
pattern LAXIndirectY = 0xB3

-- Illegal SAX
pattern SAXZeroPage, SAXZeroPageY, SAXAbsolute, SAXIndirectX :: OpCode
pattern SAXZeroPage = 0x87
pattern SAXZeroPageY = 0x97
pattern SAXAbsolute = 0x8F
pattern SAXIndirectX = 0x83

-- Illegal DCP
pattern DCPZeroPage, DCPZeroPageX, DCPAbsolute, DCPAbsoluteX, DCPAbsoluteY, DCPIndirectX, DCPIndirectY :: OpCode
pattern DCPZeroPage = 0xC7
pattern DCPZeroPageX = 0xD7
pattern DCPAbsolute = 0xCF
pattern DCPAbsoluteX = 0xDF
pattern DCPAbsoluteY = 0xDB
pattern DCPIndirectX = 0xC3
pattern DCPIndirectY = 0xD3

-- Illegal ISB
pattern ISBZeroPage, ISBZeroPageX, ISBAbsolute, ISBAbsoluteX, ISBAbsoluteY, ISBIndirectX, ISBIndirectY :: OpCode
pattern ISBZeroPage = 0xE7
pattern ISBZeroPageX = 0xF7
pattern ISBAbsolute = 0xEF
pattern ISBAbsoluteX = 0xFF
pattern ISBAbsoluteY = 0xFB
pattern ISBIndirectX = 0xE3
pattern ISBIndirectY = 0xF3

-- Illegal SLO
pattern SLOZeroPage, SLOZeroPageX, SLOAbsolute, SLOAbsoluteX, SLOAbsoluteY, SLOIndirectX, SLOIndirectY :: OpCode
pattern SLOZeroPage = 0x07
pattern SLOZeroPageX = 0x17
pattern SLOAbsolute = 0x0F
pattern SLOAbsoluteX = 0x1F
pattern SLOAbsoluteY = 0x1B
pattern SLOIndirectX = 0x03
pattern SLOIndirectY = 0x13

-- Illegal RLA
pattern RLAZeroPage, RLAZeroPageX, RLAAbsolute, RLAAbsoluteX, RLAAbsoluteY, RLAIndirectX, RLAIndirectY :: OpCode
pattern RLAZeroPage = 0x27
pattern RLAZeroPageX = 0x37
pattern RLAAbsolute = 0x2F
pattern RLAAbsoluteX = 0x3F
pattern RLAAbsoluteY = 0x3B
pattern RLAIndirectX = 0x23
pattern RLAIndirectY = 0x33

-- Illegal SRE
pattern SREZeroPage, SREZeroPageX, SREAbsolute, SREAbsoluteX, SREAbsoluteY, SREIndirectX, SREIndirectY :: OpCode
pattern SREZeroPage = 0x47
pattern SREZeroPageX = 0x57
pattern SREAbsolute = 0x4F
pattern SREAbsoluteX = 0x5F
pattern SREAbsoluteY = 0x5B
pattern SREIndirectX = 0x43
pattern SREIndirectY = 0x53

-- Illegal RRA
pattern RRAZeroPage, RRAZeroPageX, RRAAbsolute, RRAAbsoluteX, RRAAbsoluteY, RRAIndirectX, RRAIndirectY :: OpCode
pattern RRAZeroPage = 0x67
pattern RRAZeroPageX = 0x77
pattern RRAAbsolute = 0x6F
pattern RRAAbsoluteX = 0x7F
pattern RRAAbsoluteY = 0x7B
pattern RRAIndirectX = 0x63
pattern RRAIndirectY = 0x73

getOperandLength :: OpCode -> Word16
getOperandLength BRK = 0
getOperandLength TAX = 0
getOperandLength INX = 0
getOperandLength INY = 0
getOperandLength DEX = 0
getOperandLength DEY = 0
getOperandLength NOP = 0
-- LDA
getOperandLength LDAImmediate = 1
getOperandLength LDAZeroPage = 1
getOperandLength LDAZeroPageX = 1
getOperandLength LDAAbsolute = 2
getOperandLength LDAAbsoluteX = 2
getOperandLength LDAAbsoluteY = 2
getOperandLength LDAIndirectX = 1
getOperandLength LDAIndirectY = 1
-- LDX
getOperandLength LDXImmediate = 1
getOperandLength LDXZeroPage = 1
getOperandLength LDXZeroPageY = 1
getOperandLength LDXAbsolute = 2
getOperandLength LDXAbsoluteY = 2
-- LDY
getOperandLength LDYImmediate = 1
getOperandLength LDYZeroPage = 1
getOperandLength LDYZeroPageX = 1
getOperandLength LDYAbsolute = 2
getOperandLength LDYAbsoluteX = 2
-- STA
getOperandLength STAZeroPage = 1
getOperandLength STAZeroPageX = 1
getOperandLength STAAbsolute = 2
getOperandLength STAAbsoluteX = 2
getOperandLength STAAbsoluteY = 2
getOperandLength STAIndirectX = 1
getOperandLength STAIndirectY = 1
-- STX
getOperandLength STXZeroPage = 1
getOperandLength STXZeroPageY = 1
getOperandLength STXAbsolute = 2
-- STY
getOperandLength STYZeroPage = 1
getOperandLength STYZeroPageX = 1
getOperandLength STYAbsolute = 2
-- ADC
getOperandLength ADCImmediate = 1
getOperandLength ADCZeroPage = 1
getOperandLength ADCZeroPageX = 1
getOperandLength ADCAbsolute = 2
getOperandLength ADCAbsoluteX = 2
getOperandLength ADCAbsoluteY = 2
getOperandLength ADCIndirectX = 1
getOperandLength ADCIndirectY = 1
-- SBC
getOperandLength SBCImmediate = 1
getOperandLength SBCZeroPage = 1
getOperandLength SBCZeroPageX = 1
getOperandLength SBCAbsolute = 2
getOperandLength SBCAbsoluteX = 2
getOperandLength SBCAbsoluteY = 2
getOperandLength SBCIndirectX = 1
getOperandLength SBCIndirectY = 1
-- INC/DEC
getOperandLength INCZeroPage = 1
getOperandLength INCZeroPageX = 1
getOperandLength INCAbsolute = 2
getOperandLength INCAbsoluteX = 2
getOperandLength DECZeroPage = 1
getOperandLength DECZeroPageX = 1
getOperandLength DECAbsolute = 2
getOperandLength DECAbsoluteX = 2
-- Bitwise
getOperandLength ANDImmediate = 1
getOperandLength ANDZeroPage = 1
getOperandLength ANDZeroPageX = 1
getOperandLength ANDAbsolute = 2
getOperandLength ANDAbsoluteX = 2
getOperandLength ANDAbsoluteY = 2
getOperandLength ANDIndirectX = 1
getOperandLength ANDIndirectY = 1
getOperandLength EORImmediate = 1
getOperandLength EORZeroPage = 1
getOperandLength EORZeroPageX = 1
getOperandLength EORAbsolute = 2
getOperandLength EORAbsoluteX = 2
getOperandLength EORAbsoluteY = 2
getOperandLength EORIndirectX = 1
getOperandLength EORIndirectY = 1
getOperandLength ORAImmediate = 1
getOperandLength ORAZeroPage = 1
getOperandLength ORAZeroPageX = 1
getOperandLength ORAAbsolute = 2
getOperandLength ORAAbsoluteX = 2
getOperandLength ORAAbsoluteY = 2
getOperandLength ORAIndirectX = 1
getOperandLength ORAIndirectY = 1
getOperandLength BITZeroPage = 1
getOperandLength BITAbsolute = 2
-- Compare
getOperandLength CMPImmediate = 1
getOperandLength CMPZeroPage = 1
getOperandLength CMPZeroPageX = 1
getOperandLength CMPAbsolute = 2
getOperandLength CMPAbsoluteX = 2
getOperandLength CMPAbsoluteY = 2
getOperandLength CMPIndirectX = 1
getOperandLength CMPIndirectY = 1
getOperandLength CPXImmediate = 1
getOperandLength CPXZeroPage = 1
getOperandLength CPXAbsolute = 2
getOperandLength CPYImmediate = 1
getOperandLength CPYZeroPage = 1
getOperandLength CPYAbsolute = 2
-- Shifts
getOperandLength ASLAccumulator = 0
getOperandLength ASLZeroPage = 1
getOperandLength ASLZeroPageX = 1
getOperandLength ASLAbsolute = 2
getOperandLength ASLAbsoluteX = 2
getOperandLength LSRAccumulator = 0
getOperandLength LSRZeroPage = 1
getOperandLength LSRZeroPageX = 1
getOperandLength LSRAbsolute = 2
getOperandLength LSRAbsoluteX = 2
getOperandLength ROLAccumulator = 0
getOperandLength ROLZeroPage = 1
getOperandLength ROLZeroPageX = 1
getOperandLength ROLAbsolute = 2
getOperandLength ROLAbsoluteX = 2
getOperandLength RORAccumulator = 0
getOperandLength RORZeroPage = 1
getOperandLength RORZeroPageX = 1
getOperandLength RORAbsolute = 2
getOperandLength RORAbsoluteX = 2
-- Branch
getOperandLength BCC = 1
getOperandLength BCS = 1
getOperandLength BEQ = 1
getOperandLength BMI = 1
getOperandLength BNE = 1
getOperandLength BPL = 1
getOperandLength BVC = 1
getOperandLength BVS = 1
-- Jump
getOperandLength JMPAbsolute = 2
getOperandLength JMPIndirect = 2
getOperandLength JSRAbsolute = 2
getOperandLength RTS = 0
getOperandLength RTI = 0
-- Flags/Stack/Transfers
getOperandLength CLC = 0
getOperandLength CLD = 0
getOperandLength CLI = 0
getOperandLength CLV = 0
getOperandLength SEC = 0
getOperandLength SED = 0
getOperandLength SEI = 0
getOperandLength PHA = 0
getOperandLength PHP = 0
getOperandLength PLA = 0
getOperandLength PLP = 0
getOperandLength TAY = 0
getOperandLength TYA = 0
getOperandLength TSX = 0
getOperandLength TXS = 0
getOperandLength TXA = 0
-- Illegal NOPs
getOperandLength op | op `elem` [NOP_1A, NOP_3A, NOP_5A, NOP_7A, NOP_DA, NOP_FA] = 0
getOperandLength op | op `elem` [NOP_04, NOP_14, NOP_34, NOP_44, NOP_54, NOP_64, NOP_74, NOP_80, NOP_82, NOP_89, NOP_C2, NOP_E2, NOP_D4, NOP_F4] = 1
getOperandLength op | op `elem` [NOP_0C, NOP_1C, NOP_3C, NOP_5C, NOP_7C, NOP_DC, NOP_FC] = 2
getOperandLength SBC_EB = 1
-- Illegal LAX
getOperandLength LAXZeroPage = 1
getOperandLength LAXZeroPageY = 1
getOperandLength LAXAbsolute = 2
getOperandLength LAXAbsoluteY = 2
getOperandLength LAXIndirectX = 1
getOperandLength LAXIndirectY = 1
-- Illegal SAX
getOperandLength SAXZeroPage = 1
getOperandLength SAXZeroPageY = 1
getOperandLength SAXAbsolute = 2
getOperandLength SAXIndirectX = 1
-- Illegal DCP
getOperandLength DCPZeroPage = 1
getOperandLength DCPZeroPageX = 1
getOperandLength DCPAbsolute = 2
getOperandLength DCPAbsoluteX = 2
getOperandLength DCPAbsoluteY = 2
getOperandLength DCPIndirectX = 1
getOperandLength DCPIndirectY = 1
-- Illegal ISB
getOperandLength ISBZeroPage = 1
getOperandLength ISBZeroPageX = 1
getOperandLength ISBAbsolute = 2
getOperandLength ISBAbsoluteX = 2
getOperandLength ISBAbsoluteY = 2
getOperandLength ISBIndirectX = 1
getOperandLength ISBIndirectY = 1
-- Illegal SLO
getOperandLength SLOZeroPage = 1
getOperandLength SLOZeroPageX = 1
getOperandLength SLOAbsolute = 2
getOperandLength SLOAbsoluteX = 2
getOperandLength SLOAbsoluteY = 2
getOperandLength SLOIndirectX = 1
getOperandLength SLOIndirectY = 1
-- Illegal RLA
getOperandLength RLAZeroPage = 1
getOperandLength RLAZeroPageX = 1
getOperandLength RLAAbsolute = 2
getOperandLength RLAAbsoluteX = 2
getOperandLength RLAAbsoluteY = 2
getOperandLength RLAIndirectX = 1
getOperandLength RLAIndirectY = 1
-- Illegal SRE
getOperandLength SREZeroPage = 1
getOperandLength SREZeroPageX = 1
getOperandLength SREAbsolute = 2
getOperandLength SREAbsoluteX = 2
getOperandLength SREAbsoluteY = 2
getOperandLength SREIndirectX = 1
getOperandLength SREIndirectY = 1
-- Illegal RRA
getOperandLength RRAZeroPage = 1
getOperandLength RRAZeroPageX = 1
getOperandLength RRAAbsolute = 2
getOperandLength RRAAbsoluteX = 2
getOperandLength RRAAbsoluteY = 2
getOperandLength RRAIndirectX = 1
getOperandLength RRAIndirectY = 1
getOperandLength _ = error "Unknown opcode"
