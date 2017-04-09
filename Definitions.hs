module Definitions where

import Data.Binary
import Data.Int
import Data.List


data Command = ADC
             | ADD
             | AND
             | CALL
             | CMP
             | DEC
             | INC
             | JE
             | JG
             | JMP
             | LEA
             | MOV
             | OR
             | POP
             | PUSH
             | RET
             | SBB
             | SETE
             | SETNE
             | SUB
             | SYSCALL
             | TEST
             | XOR
             -- Data pseudo-commands
             | DB
             | DW
             | DD
             | DQ
             | EQU
             deriving (Eq, Show, Read)


-- Pseudo-commands that render data rather than x64 instructions
dataCommands = [DB, DW, DD, DQ]


-- Jump/call commands whose operands reference RIP-relative addresses
jumpCommands = [CALL, JE, JG, JMP]


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


intSize :: Int -> Size
intSize 1 = BYTE
intSize 2 = WORD
intSize 4 = DWORD
intSize 8 = QWORD


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
               deriving (Eq, Show, Read)


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
           deriving Show


data ImmediateDescriptor = Literal [Word8]
                         | Symbol  Size    String


data Operand = Register   Registers
             | Address    Registers Scale Registers Displacement
             | Immediate  ImmediateDescriptor
             | Relative   ImmediateDescriptor
             | Expression [OperandPart]


data OperandPart = RegisterPart Registers
                 | NumericPart  Integer
                 | SymbolPart   String
                 | ControlPart  Char
                 | QuotedPart   String
                 deriving (Eq, Show)


showOperandPart :: OperandPart -> String
showOperandPart (RegisterPart r) = "RegisterPart " ++ show r
showOperandPart (NumericPart  n) = "NumericPart " ++ show n
showOperandPart (SymbolPart   s) = "SymbolPart " ++ s
showOperandPart (ControlPart  c) = "ControlPart " ++ [c]
showOperandPart (QuotedPart   q) = "QuotedPart \"" ++ q ++ "\""


showDisplacement :: Displacement -> String
showDisplacement NoDisplacement = "NoDisplacement"
showDisplacement (Displacement8 i) = "Displacement8 " ++ show i
showDisplacement (Displacement32 i) = "Displacement32 " ++ show i
showDisplacement (DisplacementSymbol size str) = "DisplacementSymbol " ++
                                                 show size ++ " " ++ str


showOperand :: Operand -> String
showOperand (Register r) = show r
showOperand (Address b s i d) = "[" ++ show b ++ " + " ++ show s ++ " * " ++
                                show i ++ " + " ++ showDisplacement d ++ "]"
showOperand (Immediate (Literal b)) = "LIT " ++ intercalate " " (map show b)
showOperand (Immediate (Symbol size str)) = "SYM " ++ show size ++ " " ++ str
showOperand (Relative (Literal b)) = "REL " ++ intercalate " " (map show b)
showOperand (Relative (Symbol size str)) = "RELSYM " ++ show size ++ " " ++ str
showOperand (Expression p) = "EXP " ++ intercalate " " (map showOperandPart p)


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


-- A description of the possible values of an operand, based on ref.x86asm.net
data Pattern = P_r8            -- An 8-bit register
             | P_r6416         -- A 16/64-bit register
             | P_rm8           -- An 8-bit register or memory address
             | P_rm6416        -- A 16/64-bit register or memory address
             | P_r163264       -- A 16/32/64-bit register
             | P_rm163264      -- A 16/32/64-bit register or memory address
             | P_imm8          -- An 8-bit immediate
             | P_imm1632       -- A 16/32-bit immediate
             | P_imm163264     -- A 16/32/64-bit immediate
             | P_rel8          -- An 8-bit relative offset
             | P_rel1632       -- A 16/32-bit relative offset
             | P_m             -- A memory address
             | R Registers     -- A single register match
             deriving Show


data Encoding = Encoding {
    mnemonic     :: Command,
    patterns     :: [Pattern],
    prefix       :: Maybe Word8,
    prefix0f     :: Bool,
    primary      :: Word8,
    registerAdd  :: Bool,        -- Add primary opcode to register index
    secondary    :: Maybe Word8,
    opcodeExt    :: Int,         -- TODO: ?
    reverseOpers :: Bool,        -- Operands are reversed for encoding?
    default32    :: Bool,        -- This encoding defaults to 32-bit
    opExtension  :: Maybe Word8, -- ModR/M opcode extension (0-7)
    modRmByte    :: Bool,        -- Whether to encode the ModRM byte
    sizes        :: [Size]       -- Valid sizes for this instruction; empty
                                 -- list means all
}


-- Symbol table entry type (part of st_info)
data SymbolType = STT_NOTYPE
                | STT_SECTION
                | STT_FILE
                | STT_FUNC
                deriving (Eq, Show)


-- Get the size/width of a register
registerSize :: Registers -> Size
registerSize r
    | elem r registers8  = BYTE
    | elem r registers16 = WORD
    | elem r registers64 = QWORD
    | otherwise          = DWORD


data Directive = Global SymbolType    String
               | Equ    [OperandPart] String
               deriving Eq


showInstruction :: Instruction -> String
showInstruction inst = do
    let labels = if null (labelNames inst) then ""
                 else "  " ++ intercalate ": " (labelNames inst) ++ ":\n"
    let size = if sizeHint inst == NoSize then ""
               else (show (sizeHint inst) ++ " ")
    labels ++ "    " ++ show (command inst) ++ " " ++ size ++
        intercalate ", " (map showOperand (operands inst))
