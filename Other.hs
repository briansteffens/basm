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
               | RIP  | EIP
               deriving Eq

registers8 = [AL, BL, CL, DL,
              AH, BH, CH, DH,
              BPL, SPL, SIL, DIL,
              R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]

extendedRegisters = [R8,  R9,  R10,  R11,  R12,  R13,  R14,  R15,
                     R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D,
                     R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W,
                     R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]

data Displacement = NoDisplacement
                  | Displacement8  Int
                  | Displacement32 Int

data Scale = NoScale
           | Scale2
           | Scale4
           | Scale8

data Operand = Register  Registers
             | Address   Registers Scale Registers Displacement
             | Immediate [Int]

data Instruction = Instruction {
    source   :: String,
    labels   :: [String],
    command  :: Command,
    operands :: [Operand]
}


-- Get the least significant 3 bits for a register.
registerIndex :: Registers -> [Int]
registerIndex r
    | elem r [AL, AX, EAX, RAX, R8B, R8W, R8D, R8]          = [0, 0, 0]
    | elem r [CL, CX, ECX, RCX, R9B, R9W, R9D, R9]          = [0, 0, 1]
    | elem r [DL, DX, EDX, RDX, R10B, R10W, R10D, R10]      = [0, 1, 0]
    | elem r [BL, BX, EBX, RBX, R11B, R11W, R11D, R11]      = [0, 1, 1]
    | elem r [AH, SPL, SP, ESP, RSP, R12B, R12W, R12D, R12] = [1, 0, 0]
    | elem r [CH, BPL, BP, EBP, RBP, R13B, R13W, R13D, R13] = [1, 0, 1]
    | elem r [DH, SIL, SI, ESI, RSI, R14B, R14W, R14D, R14] = [1, 1, 0]
    | elem r [BH, DIL, DI, EDI, RDI, R15B, R15W, R15D, R15] = [1, 1, 1]


-- The REX byte extension bit is 1 for extended registers and 0 for standard.
rexBit :: Registers -> Int
rexBit r
    | elem r extendedRegisters = 1
    | otherwise                = 0


-- Get the ModR/M bits for the given register. Returns 4 bits. The first bit is
-- the REX portion of the encoding. The last 3 bits are the ModR/M portion.
encodeRegister :: Registers -> [Int]
encodeRegister r = [rexBit r] ++ registerIndex r


-- Get the operand directionality for an opcode. Controls the order in which
-- operands are encoded in the ModR/M byte. 0 is normal, 1 is reversed.
directionality :: Int -> Int
directionality op
    | elem op [0x02, 0x03] = 1
    | otherwise            = 0


-- Reorder operands based on opcode directionality (flipping them if needed).
encodedOrder :: Int -> [Operand] -> [Operand]
encodedOrder op operands
    | directionality op == 1 = reverse operands
    | otherwise              = operands


-- Generate the first two bits of ModR/M field.
-- Operands must be in encoded order.
modBits :: [Operand] -> [Int]
modBits [Address  _   _ _  NoDisplacement   , _] = [0, 0]
modBits [Address  RIP _ _ (Displacement32 _), _] = [0, 0]
modBits [Address  EIP _ _ (Displacement32 _), _] = [0, 0]
modBits [Address  _   _ _ (Displacement8  _), _] = [0, 1]
modBits [Address  _   _ _ (Displacement32 _), _] = [1, 0]
modBits [Register _                         , _] = [1, 1]


-- Generate the R/M bits of a ModR/M byte.
rmBits :: Operand -> [Int]
rmBits (Register r                                        ) = registerIndex r
rmBits (Address  RIP NoScale NoRegister (Displacement32 _)) = [1, 0, 1]
rmBits (Address  EIP NoScale NoRegister (Displacement32 _)) = [1, 0, 1]
rmBits (Address  r   NoScale NoRegister _                 ) = registerIndex r
rmBits (Address  _   _       _          _                 ) = [1, 0, 0] -- SIB


-- Generate the REG bits of a ModR/M byte.
regBits :: Operand -> [Int]
regBits (Register r) = registerIndex r
regBits _            = [0, 0, 0]


-- Generate a ModR/M byte.
-- Operands must be in encoded order.
modRmByte :: [Operand] -> [Int]
modRmByte [op1, op2] = modBits [op1, op2] ++ regBits op2 ++ rmBits op1


-- Get an opcode based on an assembly command and its operators.
opcode :: Command -> [Operand] -> Int
opcode ADD [_,               Register r]
    | elem r registers8                       = 0x00 -- r8, r
    | otherwise                               = 0x01 -- *, r
opcode ADD [Register r,      Address _ _ _ _]
    | elem r registers8                       = 0x02 -- r8, addr
    | otherwise                               = 0x03 -- r!8, addr
opcode ADD [Register AL,     Immediate _]     = 0x04 -- AL, imm
opcode ADD [Register AX,     Immediate _]     = 0x05 -- AX, imm
opcode ADD [Register EAX,    Immediate _]     = 0x05 -- EAX, imm
opcode ADD [Register RAX,    Immediate _]     = 0x05 -- RAX, imm
opcode ADD [Address _ _ _ _, Immediate [_]]   = 0x80 -- addr, imm8
opcode ADD [Register r,      Immediate [_]]
    | elem r registers8                       = 0x80 -- r8, imm8
    | otherwise                               = 0x83 -- r!8, imm8
opcode ADD [_,               Immediate _]     = 0x81 -- *, imm


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
                 Address RAX NoScale NoRegister NoDisplacement,
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
                 Address RAX NoScale NoRegister NoDisplacement,
                 Register RBX
             ]
         },
         Instruction {
             source = "add bl, [rax]",
             labels = [],
             command = ADD,
             operands = [
                 Register BL,
                 Address RAX NoScale NoRegister NoDisplacement
             ]
         },
         Instruction {
             source = "add qword rbx, [rax]",
             labels = [],
             command = ADD,
             operands = [
                 Register RBX,
                 Address RAX NoScale NoRegister NoDisplacement
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
                 Address RCX Scale8 RDX (Displacement32 456),
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

    let go i = do
        let op = opcode (command i) (operands i)
        (showHex op " ") ++
            (show (modRmByte (encodedOrder op (operands i)))) ++
            "\t" ++
            (source i)

    putStrLn (intercalate "\n" (map go instructions))
