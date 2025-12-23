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

pattern ANDImmediate :: OpCode
pattern ANDImmediate = 0x29

pattern ANDZeroPage :: OpCode
pattern ANDZeroPage = 0x25

pattern ANDZeroPageX :: OpCode
pattern ANDZeroPageX = 0x35

pattern ANDAbsolute :: OpCode
pattern ANDAbsolute = 0x2D

pattern ANDAbsoluteX :: OpCode
pattern ANDAbsoluteX = 0x3D

pattern ANDAbsoluteY :: OpCode
pattern ANDAbsoluteY = 0x39

pattern ANDIndirectX :: OpCode
pattern ANDIndirectX = 0x21

pattern ANDIndirectY :: OpCode
pattern ANDIndirectY = 0x31

pattern EORImmediate :: OpCode
pattern EORImmediate = 0x49

pattern EORZeroPage :: OpCode
pattern EORZeroPage = 0x45

pattern EORZeroPageX :: OpCode
pattern EORZeroPageX = 0x55

pattern EORAbsolute :: OpCode
pattern EORAbsolute = 0x4D

pattern EORAbsoluteX :: OpCode
pattern EORAbsoluteX = 0x5D

pattern EORAbsoluteY :: OpCode
pattern EORAbsoluteY = 0x59

pattern EORIndirectX :: OpCode
pattern EORIndirectX = 0x41

pattern EORIndirectY :: OpCode
pattern EORIndirectY = 0x51

pattern ORAImmediate :: OpCode
pattern ORAImmediate = 0x09

pattern ORAZeroPage :: OpCode
pattern ORAZeroPage = 0x05

pattern ORAZeroPageX :: OpCode
pattern ORAZeroPageX = 0x15

pattern ORAAbsolute :: OpCode
pattern ORAAbsolute = 0x0D

pattern ORAAbsoluteX :: OpCode
pattern ORAAbsoluteX = 0x1D

pattern ORAAbsoluteY :: OpCode
pattern ORAAbsoluteY = 0x19

pattern ORAIndirectX :: OpCode
pattern ORAIndirectX = 0x01

pattern ORAIndirectY :: OpCode
pattern ORAIndirectY = 0x11

pattern BITZeroPage :: OpCode
pattern BITZeroPage = 0x24

pattern BITAbsolute :: OpCode
pattern BITAbsolute = 0x2C

pattern CMPImmediate :: OpCode
pattern CMPImmediate = 0xC9

pattern CMPZeroPage :: OpCode
pattern CMPZeroPage = 0xC5

pattern CMPZeroPageX :: OpCode
pattern CMPZeroPageX = 0xD5

pattern CMPAbsolute :: OpCode
pattern CMPAbsolute = 0xCD

pattern CMPAbsoluteX :: OpCode
pattern CMPAbsoluteX = 0xDD

pattern CMPAbsoluteY :: OpCode
pattern CMPAbsoluteY = 0xD9

pattern CMPIndirectX :: OpCode
pattern CMPIndirectX = 0xC1

pattern CMPIndirectY :: OpCode
pattern CMPIndirectY = 0xD1

pattern CPXImmediate :: OpCode
pattern CPXImmediate = 0xE0

pattern CPXZeroPage :: OpCode
pattern CPXZeroPage = 0xE4

pattern CPXAbsolute :: OpCode
pattern CPXAbsolute = 0xEC

pattern CPYImmediate :: OpCode
pattern CPYImmediate = 0xC0

pattern CPYZeroPage :: OpCode
pattern CPYZeroPage = 0xC4

pattern CPYAbsolute :: OpCode
pattern CPYAbsolute = 0xCC

pattern ASLAccumulator :: OpCode
pattern ASLAccumulator = 0x0A

pattern ASLZeroPage :: OpCode
pattern ASLZeroPage = 0x06

pattern ASLZeroPageX :: OpCode
pattern ASLZeroPageX = 0x16

pattern ASLAbsolute :: OpCode
pattern ASLAbsolute = 0x0E

pattern ASLAbsoluteX :: OpCode
pattern ASLAbsoluteX = 0x1E

pattern LSRAccumulator :: OpCode
pattern LSRAccumulator = 0x4A

pattern LSRZeroPage :: OpCode
pattern LSRZeroPage = 0x46

pattern LSRZeroPageX :: OpCode
pattern LSRZeroPageX = 0x56

pattern LSRAbsolute :: OpCode
pattern LSRAbsolute = 0x4E

pattern LSRAbsoluteX :: OpCode
pattern LSRAbsoluteX = 0x5E

pattern ROLAccumulator :: OpCode
pattern ROLAccumulator = 0x2A

pattern ROLZeroPage :: OpCode
pattern ROLZeroPage = 0x26

pattern ROLZeroPageX :: OpCode
pattern ROLZeroPageX = 0x36

pattern ROLAbsolute :: OpCode
pattern ROLAbsolute = 0x2E

pattern ROLAbsoluteX :: OpCode
pattern ROLAbsoluteX = 0x3E

pattern RORAccumulator :: OpCode
pattern RORAccumulator = 0x6A

pattern RORZeroPage :: OpCode
pattern RORZeroPage = 0x66

pattern RORZeroPageX :: OpCode
pattern RORZeroPageX = 0x76

pattern RORAbsolute :: OpCode
pattern RORAbsolute = 0x6E

pattern RORAbsoluteX :: OpCode
pattern RORAbsoluteX = 0x7E

pattern BCC :: OpCode
pattern BCC = 0x90

pattern BCS :: OpCode
pattern BCS = 0xB0

pattern BEQ :: OpCode
pattern BEQ = 0xF0

pattern BMI :: OpCode
pattern BMI = 0x30

pattern BNE :: OpCode
pattern BNE = 0xD0

pattern BPL :: OpCode
pattern BPL = 0x10

pattern BVC :: OpCode
pattern BVC = 0x50

pattern BVS :: OpCode
pattern BVS = 0x70

pattern JMPAbsolute :: OpCode
pattern JMPAbsolute = 0x4C

pattern JMPIndirect :: OpCode
pattern JMPIndirect = 0x6C

pattern JSRAbsolute :: OpCode
pattern JSRAbsolute = 0x20

pattern RTS :: OpCode
pattern RTS = 0x60

pattern RTI :: OpCode
pattern RTI = 0x40

pattern CLC :: OpCode
pattern CLC = 0x18

pattern CLD :: OpCode
pattern CLD = 0xD8

pattern CLI :: OpCode
pattern CLI = 0x58

pattern CLV :: OpCode
pattern CLV = 0xB8

pattern SEC :: OpCode
pattern SEC = 0x38

pattern SED :: OpCode
pattern SED = 0xF8

pattern SEI :: OpCode
pattern SEI = 0x78

pattern PHA :: OpCode
pattern PHA = 0x48

pattern PHP :: OpCode
pattern PHP = 0x08

pattern PLA :: OpCode
pattern PLA = 0x68

pattern PLP :: OpCode
pattern PLP = 0x28

pattern TAY :: OpCode
pattern TAY = 0xA8

pattern TYA :: OpCode
pattern TYA = 0x98

pattern TSX :: OpCode
pattern TSX = 0xBA

pattern TXS :: OpCode
pattern TXS = 0x9A

pattern TXA :: OpCode
pattern TXA = 0x8A

pattern TYA_ :: OpCode
pattern TYA_ = 0x98

pattern NOP :: OpCode
pattern NOP = 0xEA

getOperandLength :: OpCode -> Word16
getOperandLength BRK            = 0
getOperandLength TAX            = 0
getOperandLength INX            = 0
getOperandLength LDAImmediate   = 1
getOperandLength LDAZeroPage    = 1
getOperandLength LDAZeroPageX   = 1
getOperandLength LDAAbsolute    = 2
getOperandLength LDAAbsoluteX   = 2
getOperandLength LDAAbsoluteY   = 2
getOperandLength LDAIndirectX   = 1
getOperandLength LDAIndirectY   = 1
getOperandLength LDXImmediate   = 1
getOperandLength LDXZeroPage    = 1
getOperandLength LDXZeroPageY   = 1
getOperandLength LDXAbsolute    = 2
getOperandLength LDXAbsoluteY   = 2
getOperandLength LDYImmediate   = 1
getOperandLength STAZeroPage    = 1
getOperandLength STAZeroPageX   = 1
getOperandLength STAAbsolute    = 2
getOperandLength STAAbsoluteX   = 2
getOperandLength STAAbsoluteY   = 2
getOperandLength STAIndirectX   = 1
getOperandLength STAIndirectY   = 1
getOperandLength ADCImmediate   = 1
getOperandLength ADCZeroPage    = 1
getOperandLength ADCZeroPageX   = 1
getOperandLength ADCAbsolute    = 2
getOperandLength ADCAbsoluteX   = 2
getOperandLength ADCAbsoluteY   = 2
getOperandLength ADCIndirectX   = 1
getOperandLength ADCIndirectY   = 1
getOperandLength SBCImmediate   = 1
getOperandLength SBCZeroPage    = 1
getOperandLength SBCZeroPageX   = 1
getOperandLength SBCAbsolute    = 2
getOperandLength SBCAbsoluteX   = 2
getOperandLength SBCAbsoluteY   = 2
getOperandLength SBCIndirectX   = 1
getOperandLength SBCIndirectY   = 1
getOperandLength DEX            = 0
getOperandLength DEY            = 0
getOperandLength INCZeroPage    = 1
getOperandLength INCZeroPageX   = 1
getOperandLength INCAbsolute    = 2
getOperandLength INCAbsoluteX   = 2
getOperandLength DECZeroPage    = 1
getOperandLength DECZeroPageX   = 1
getOperandLength DECAbsolute    = 2
getOperandLength DECAbsoluteX   = 2
getOperandLength ANDImmediate   = 1
getOperandLength ANDZeroPage    = 1
getOperandLength ANDZeroPageX   = 1
getOperandLength ANDAbsolute    = 2
getOperandLength ANDAbsoluteX   = 2
getOperandLength ANDAbsoluteY   = 2
getOperandLength ANDIndirectX   = 1
getOperandLength ANDIndirectY   = 1
getOperandLength EORImmediate   = 1
getOperandLength EORZeroPage    = 1
getOperandLength EORZeroPageX   = 1
getOperandLength EORAbsolute    = 2
getOperandLength EORAbsoluteX   = 2
getOperandLength EORAbsoluteY   = 2
getOperandLength EORIndirectX   = 1
getOperandLength EORIndirectY   = 1
getOperandLength ORAImmediate   = 1
getOperandLength ORAZeroPage    = 1
getOperandLength ORAZeroPageX   = 1
getOperandLength ORAAbsolute    = 2
getOperandLength ORAAbsoluteX   = 2
getOperandLength ORAAbsoluteY   = 2
getOperandLength ORAIndirectX   = 1
getOperandLength ORAIndirectY   = 1
getOperandLength BITZeroPage    = 1
getOperandLength BITAbsolute    = 2
getOperandLength CMPImmediate   = 1
getOperandLength CMPZeroPage    = 1
getOperandLength CMPZeroPageX   = 1
getOperandLength CMPAbsolute    = 2
getOperandLength CMPAbsoluteX   = 2
getOperandLength CMPAbsoluteY   = 2
getOperandLength CMPIndirectX   = 1
getOperandLength CMPIndirectY   = 1
getOperandLength CPXImmediate   = 1
getOperandLength CPXZeroPage    = 1
getOperandLength CPXAbsolute    = 2
getOperandLength CPYImmediate   = 1
getOperandLength CPYZeroPage    = 1
getOperandLength CPYAbsolute    = 2
getOperandLength ASLAccumulator = 0
getOperandLength ASLZeroPage    = 1
getOperandLength ASLZeroPageX   = 1
getOperandLength ASLAbsolute    = 2
getOperandLength ASLAbsoluteX   = 2
getOperandLength LSRAccumulator = 0
getOperandLength LSRZeroPage    = 1
getOperandLength LSRZeroPageX   = 1
getOperandLength LSRAbsolute    = 2
getOperandLength LSRAbsoluteX   = 2
getOperandLength ROLAccumulator = 0
getOperandLength ROLZeroPage    = 1
getOperandLength ROLZeroPageX   = 1
getOperandLength ROLAbsolute    = 2
getOperandLength ROLAbsoluteX   = 2
getOperandLength RORAccumulator = 0
getOperandLength RORZeroPage    = 1
getOperandLength RORZeroPageX   = 1
getOperandLength RORAbsolute    = 2
getOperandLength RORAbsoluteX   = 2
getOperandLength BCC            = 1
getOperandLength BCS            = 1
getOperandLength BEQ            = 1
getOperandLength BMI            = 1
getOperandLength BNE            = 1
getOperandLength BPL            = 1
getOperandLength BVC            = 1
getOperandLength BVS            = 1
getOperandLength JMPAbsolute    = 2
getOperandLength JMPIndirect    = 2
getOperandLength JSRAbsolute    = 2
getOperandLength RTS            = 0
getOperandLength RTI            = 0
getOperandLength CLC            = 0
getOperandLength CLD            = 0
getOperandLength CLI            = 0
getOperandLength CLV            = 0
getOperandLength SEC            = 0
getOperandLength SED            = 0
getOperandLength SEI            = 0
getOperandLength PHA            = 0
getOperandLength PHP            = 0
getOperandLength PLA            = 0
getOperandLength PLP            = 0
getOperandLength TAY            = 0
getOperandLength TYA            = 0
getOperandLength TSX            = 0
getOperandLength TXS            = 0
getOperandLength TXA            = 0
getOperandLength NOP            = 0
getOperandLength _              = error "Unknown opcode"
