module Main where

import Data.List
import qualified Data.ByteString as B
import Numeric (showHex)

data Command = ADD

data Size = BYTE
          | WORD
          | DWORD
          | QWORD

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
               deriving Eq

registers8 = [AL, BL, CL, DL,
              AH, BH, CH, DH,
              BPL, SPL, SIL, DIL,
              R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]

data Displacement = Displacement8  Int
                  | Displacement16 Int
                  | Displacement32 Int

data Scale = Scale1
           | Scale2
           | Scale4
           | Scale8

data AddressOperand = AddressOperand {
    base         :: Registers,
    index        :: Registers,
    scale        :: Scale,
    displacement :: Displacement
}

data Operand = Register  Registers
             | Address   Registers Scale Registers Displacement
             | Immediate [Int]

data Instruction = Instruction {
    source   :: String,
    labels   :: [String],
    command  :: Command,
    operands :: [Operand]
}

opcode :: Command -> [Operand] -> Int
opcode ADD [_,               Register r]
    | elem r registers8                       = 0x00
    | otherwise                               = 0x01
opcode ADD [Register r,      Address _ _ _ _]
    | elem r registers8                       = 0x02
    | otherwise                               = 0x03
opcode ADD [Register AL,     Immediate _]     = 0x04
opcode ADD [Register AX,     Immediate _]     = 0x05
opcode ADD [Register EAX,    Immediate _]     = 0x05
opcode ADD [Register RAX,    Immediate _]     = 0x05
opcode ADD [Address _ _ _ _, Immediate [_]]   = 0x80
opcode ADD [Register r,      Immediate [_]]
    | elem r registers8                       = 0x80
    | otherwise                               = 0x83
opcode ADD [_,               Immediate _]     = 0x81

main :: IO ()
main = do
    let instructions = [
         Instruction {
             source = "add byte al, bl",
             labels = [],
             command = ADD,
             operands = [
                 Register AL,
                 Register BL
             ]
         },
         Instruction {
             source = "add byte [rax], bl",
             labels = [],
             command = ADD,
             operands = [
                 Address RAX Scale1 NoRegister (Displacement8 0),
                 Register BL
             ]
         },
         Instruction {
             source = "add qword rax, rbx",
             labels = [],
             command = ADD,
             operands = [
                 Register RAX,
                 Register RBX
             ]
         },
         Instruction {
             source = "add qword [rax], rbx",
             labels = [],
             command = ADD,
             operands = [
                 Address RAX Scale1 NoRegister (Displacement8 0),
                 Register RBX
             ]
         },
         Instruction {
             source = "add bl, [rax]",
             labels = [],
             command = ADD,
             operands = [
                 Register BL,
                 Address RAX Scale1 NoRegister (Displacement8 0)
             ]
         },
         Instruction {
             source = "add qword rbx, [rax]",
             labels = [],
             command = ADD,
             operands = [
                 Register RBX,
                 Address RAX Scale1 NoRegister (Displacement8 0)
             ]
         },
         Instruction {
             source = "add byte al, 123",
             labels = [],
             command = ADD,
             operands = [
                 Register AL,
                 Immediate [123]
             ]
         },
         Instruction {
             source = "add dword eax, 123",
             labels = [],
             command = ADD,
             operands = [
                 Register EAX,
                 Immediate [123]
             ]
         },
         Instruction {
             source = "add byte ch, 123",
             labels = [],
             command = ADD,
             operands = [
                 Register CH,
                 Immediate [123]
             ]
         },
         Instruction {
             source = "add qword [rcx + 8 * rdx + 456], 456",
             labels = [],
             command = ADD,
             operands = [
                 Address RCX Scale8 RDX (Displacement8 456),
                 Immediate [0x01, 0xc8]
             ]
         },
         Instruction {
             source = "add rbx, 123",
             labels = [],
             command = ADD,
             operands = [
                 Register RBX,
                 Immediate [123]
             ]
         }]

    let go i = (showHex (opcode (command i) (operands i)) "\t") ++
               (source i)

    putStrLn (intercalate "\n" (map go instructions))
