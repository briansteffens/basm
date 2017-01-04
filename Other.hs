module Main where

import Data.Int
import Data.List
import Data.Binary
import qualified Data.ByteString as B

import Numeric (showHex)
import Debug.Trace (trace)

import Shared


data Result a = Success a
              | Warn String a
              | Fail String


data Command = ADD


data Size = BYTE
          | WORD
          | DWORD
          | QWORD
          | NoSize
          deriving (Eq, Show)


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
                  | Displacement8  Int8
                  | Displacement32 Int32


data Scale = NoScale
           | Scale2
           | Scale4
           | Scale8


data Operand = Register  Registers
             | Address   Registers Scale Registers Displacement
             | Immediate [Word8]


data Instruction = Instruction {
    source   :: String,
    labels   :: [String],
    sizeHint :: Size,
    command  :: Command,
    operands :: [Operand]
}


-- Opcodes that default to 32-bit operation size without any prefix bytes
ops32 = [0x01, 0x03, 0x05, 0x81, 0x83]


-- Check if an operand references any of a list of registers
anyRegisters :: Operand -> [Registers] -> Bool
anyRegisters (Immediate _        ) _    = False
anyRegisters (Register  r        ) regs = elem r  regs
anyRegisters (Address   r1 _ r2 _) regs = elem r1 regs || elem r2 regs


-- Check if an operand references any extended registers
anyExtendedRegisters :: Operand -> Bool
anyExtendedRegisters operand = anyRegisters operand extendedRegisters


-- Check if an operand references any high-byte registers
anyHigh8Registers :: Operand -> Bool
anyHigh8Registers operand = anyRegisters operand registersHigh8


-- The optional extension bit for encoding a register.
exBit :: Registers -> Int
exBit r = if elem r extendedRegisters then 1 else 0


-- Generate the extension bit of the MODRM.reg field.
-- Operands must be in encoded order.
extensionR :: [Operand] -> Int
extensionR [_, Register r] = exBit r
extensionR [_, _         ] = 0


-- Generate the extension bit of the SIB.index field.
-- Operands must be in encoded order.
extensionX :: [Operand] -> Int
extensionX [Address _ _ r _, _] = exBit r
extensionX [_              , _] = 0


-- Generate the extension bit of the MODRM.rm or SIB.base field.
-- Operands must be in encoded order.
extensionB :: [Operand] -> Int
extensionB [Register r      , _] = exBit r
extensionB [Address  r _ _ _, _] = exBit r
extensionB [_               , _] = 0


-- Encode a REX byte if necessary.
-- Operands must be in encoded order.
-- TODO: Make sure extended registers and high byte registers aren't mixed
rex :: Int -> Size -> [Operand] -> [Word8]
rex op size [op1, op2]
    | (size == QWORD && elem op ops32) ||
      anyExtendedRegisters op1 ||
      anyExtendedRegisters op2 = [bitsToByte [0, 1, 0, 0,
                                              if size == QWORD then 1 else 0,
                                              extensionR [op1, op2],
                                              extensionX [op1, op2],
                                              extensionB [op1, op2]]]
    | otherwise                = []


-- Encode a size override prefix if necessary.
prefixSize16 :: Int -> Size -> [Word8]
prefixSize16 op size
    | elem op ops32 && size == WORD = [0x66]
    | otherwise                     = []


-- Encode any necessary sizing prefixes for an instruction
-- Operands must be in encoded order.
sizePrefix :: Int -> Size -> [Operand] -> [Word8]
sizePrefix op size operands = (prefixSize16 op size) ++
                              (rex op size operands)


-- Get the size/width of a register
registerSize :: Registers -> Size
registerSize r
    | elem r registers8  = BYTE
    | elem r registers16 = WORD
    | elem r registers64 = QWORD
    | otherwise          = DWORD


-- Get the size/width of an operand
operandSize :: Operand -> Maybe Size
operandSize (Register r         ) = Just (registerSize r)
operandSize (Immediate [_]      ) = Just BYTE             -- imm8
operandSize (Immediate [_,_,_,_]) = Just DWORD            -- imm32
operandSize (Address    _ _ _ _ ) = Nothing


-- Infer the operation size based on its operands, if possible.
inferOpSize :: [Operand] -> Result Size
inferOpSize [Address _ _ _ _, Immediate _    ] = Success NoSize
inferOpSize [Register r     , Immediate _    ] = Success (registerSize r)
inferOpSize [Address _ _ _ _, Register r     ] = Success (registerSize r)
inferOpSize [Register r     , Address _ _ _ _] = Success (registerSize r)
inferOpSize [Register r1    , Register r2    ]
    | size1 == size2                           = Success size1
    | otherwise                                = Fail "Operand size mismatch"
    where size1 = registerSize r1
          size2 = registerSize r2


-- Given an operation size and a size hint, work out which takes precedence.
applySizeHint :: Result Size -> Size -> Result Size
applySizeHint (Success NoSize) NoSize      = Fail "Missing operand size"
applySizeHint (Success NoSize) hint        = Success hint
applySizeHint (Success size)   NoSize      = Success size
applySizeHint (Success size)   hint
    | size == hint                         = Success size
    | otherwise                            = Warn "Size hint ignored" size


-- Sort out an operation size given its operands and size hint.
opSize :: Size -> [Operand] -> Result Size
opSize hint operands = applySizeHint (inferOpSize operands) hint


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


-- Generate a ModR/M byte if necessary.
-- Operands must be in encoded order.
modRmByte :: Int -> [Operand] -> [Word8]
modRmByte op [op1, op2]
    | elem op [0x04, 0x05] = []
    | otherwise            = [bitsToByte (modBits [op1, op2] ++
                                          regBits op2 ++
                                          rmBits op1)]


-- Convert a scale into its 2-bit SIB byte representation.
encodeScale :: Scale -> [Int]
encodeScale NoScale = [0, 0]
encodeScale Scale2  = [0, 1]
encodeScale Scale4  = [1, 0]
encodeScale Scale8  = [1, 1]


-- Encode a SIB byte if necessary.
-- Operands must be in encoded order.
sibByte :: [Operand] -> [Word8]
sibByte [Address _    NoScale NoRegister _, _] = []
sibByte [Address base scale   index      _, _] =
    [bitsToByte (encodeScale scale ++
                 registerIndex index ++
                 registerIndex base)]
sibByte [_, _]                                 = []


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
             sizeHint = BYTE,
             operands = [
                 Register AL,
                 Register BL
             ]
         },
         Instruction {
             source = "add byte [rax], bl",
             labels = [],
             command = ADD,
             sizeHint = BYTE,
             operands = [
                 Address RAX NoScale NoRegister NoDisplacement,
                 Register BL
             ]
         },
         Instruction {
             source = "add qword rax, rbx",
             labels = [],
             command = ADD,
             sizeHint = QWORD,
             operands = [
                 Register RAX,
                 Register RBX
             ]
         },
         Instruction {
             source = "add qword [rax], rbx",
             labels = [],
             command = ADD,
             sizeHint = QWORD,
             operands = [
                 Address RAX NoScale NoRegister NoDisplacement,
                 Register RBX
             ]
         },
         Instruction {
             source = "add bl, [rax]",
             labels = [],
             command = ADD,
             sizeHint = NoSize,
             operands = [
                 Register BL,
                 Address RAX NoScale NoRegister NoDisplacement
             ]
         },
         Instruction {
             source = "add qword rbx, [rax]",
             labels = [],
             command = ADD,
             sizeHint = QWORD,
             operands = [
                 Register RBX,
                 Address RAX NoScale NoRegister NoDisplacement
             ]
         },
         Instruction {
             source = "add byte al, 123",
             labels = [],
             command = ADD,
             sizeHint = BYTE,
             operands = [
                 Register AL,
                 Immediate (toBytes (123 :: Word8))
             ]
         },
         Instruction {
             source = "add dword eax, 123",
             labels = [],
             command = ADD,
             sizeHint = DWORD,
             operands = [
                 Register EAX,
                 Immediate (toBytes (123 :: Word8))
             ]
         },
         Instruction {
             source = "add byte ch, 123",
             labels = [],
             command = ADD,
             sizeHint = BYTE,
             operands = [
                 Register CH,
                 Immediate (toBytes (123 :: Word8))
             ]
         },
         Instruction {
             source = "add qword [rcx + 8 * rdx + 456], 456",
             labels = [],
             command = ADD,
             sizeHint = QWORD,
             operands = [
                 Address RCX Scale8 RDX (Displacement32 456),
                 Immediate (toBytes (456 :: Word16))
             ]
         },
         Instruction {
             source = "add rbx, 123",
             labels = [],
             command = ADD,
             sizeHint = NoSize,
             operands = [
                 Register RBX,
                 Immediate (toBytes (123 :: Word8))
             ]
         },
         Instruction {
             source = "add ax, bx",
             labels = [],
             command = ADD,
             sizeHint = NoSize,
             operands = [
                 Register AX,
                 Register BX
             ]
         },
         Instruction {
             source = "add ax, r8w",
             labels = [],
             command = ADD,
             sizeHint = NoSize,
             operands = [
                 Register AX,
                 Register R8W
             ]
         }]
         --},
         --Instruction {
         --    source = "add [rbx], 123",
         --    labels = [],
         --    command = ADD,
         --    sizeHint = NoSize,
         --    operands = [
         --        Address RBX NoScale NoRegister NoDisplacement,
         --        Immediate [123]
         --    ]
         --},
         --Instruction {
         --    source = "add rbx, 123",
         --    labels = [],
         --    command = ADD,
         --    sizeHint = DWORD,
         --    operands = [
         --        Register RBX,
         --        Immediate [123]
         --    ]
         --}

    let go i = do
        let op = opcode (command i) (operands i)
        let size = case opSize (sizeHint i) (operands i) of
                  Success   s -> s
                  Warn    m s -> trace m s
                  Fail    m   -> error m
        let prefix = sizePrefix op size (operands i)
        let sib = sibByte (encodedOrder op (operands i))
        (source i) ++ "\n" ++
            "\tsize  : " ++ (show size) ++ "\n" ++
            "\tprefix: " ++ (intercalate " " (map show prefix)) ++ "\n" ++
            "\top    : " ++ (showHex op " ") ++ "\n" ++
            "\tmodrm : " ++ (show (modRmByte op (encodedOrder op (operands i)))) ++ "\n" ++
            "\tsib   : " ++ (intercalate " " (map show sib)) ++ "\n"

    putStrLn (intercalate "\n" (map go instructions))
