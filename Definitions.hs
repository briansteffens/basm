module Definitions where

import Data.Int
import Data.Binary


data Command = ADD


data Size = BYTE
          | WORD
          | DWORD
          | QWORD
          | NoSize
          deriving (Eq, Show)


sizeInt :: Size -> Int
sizeInt BYTE  = 1
sizeInt WORD  = 2
sizeInt DWORD = 4
sizeInt QWORD = 8


data Registers = NoRegister
               | RAX  | RBX  | RCX  | RDX  | RBP  | RSP  | RSI  | RDI
               | EAX  | EBX  | ECX  | EDX  | EBP  | ESP  | ESI  | EDI
               |  AX  |  BX  |  CX  |  DX  |  BP  |  SP  |  SI  |  DI
               |  AL  |  BL  |  CL  |  DL  |  BPL |  SPL |  SIL |  DIL
               |  AH  |  BH  |  CH  |  DH
               | R8   | R9   | R10  | R11  | R12  | R13  | R14  | R15
               | R8D  | R9D  | R10D | R11D | R12D | R13D | R14D | R15D
               | R8W  | R9W  | R10W | R11W | R12W | R13W | R14W | R15W
               | R8B  | R9B  | R10B | R11B | R12B | R13B | R14B | R15B
               | RIP  | EIP
               deriving Eq


registersHigh8 = [AH, BH, CH, DH]


registers8 = registersHigh8 ++
             [AL, BL, CL, DL,
              BPL, SPL, SIL, DIL,
              R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]


registers16 = [AX, BX, CX, DX, BP, SP, SI, DI,
               R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W]


extendedRegisters = [R8,  R9,  R10,  R11,  R12,  R13,  R14,  R15,
                     R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,
                     R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,
                     R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]


registers64 = [RAX, RBX, RCX, RDX, RBP, RSP, RSI, RDI, RIP] ++
              extendedRegisters


data Displacement = NoDisplacement
                  | Displacement8       Int8
                  | Displacement32      Int32
                  | DisplacementSymbol  Size  String


data Scale = NoScale
           | Scale2
           | Scale4
           | Scale8


data ImmediateDescriptor = Literal [Word8]
                         | Symbol  Size    String


data Operand = Register  Registers
             | Address   Registers Scale Registers Displacement
             | Immediate ImmediateDescriptor


data Instruction = Instruction {
    source     :: String,
    labelNames :: [String],
    sizeHint   :: Size,
    command    :: Command,
    operands   :: [Operand]
}


data CodeSection = CodeSection {
    sectionName  :: String,
    instructions :: [Instruction]
}
