module Definitions where

import Data.Binary
import Data.Char
import Data.Int
import Data.List
import Text.Show.Functions
import qualified Text.Read as TR

import Debug.Trace (trace)

import Shared


-- Render a human-readable string, not evaluatable code
class Display a where
    display :: a -> String


data Command =
      ADC
    | ADD
    | AND
    | CALL
    | CMP
    | DEC
    | IDIV
    | INC
    | JE
    | JG
    | JMP
    | LEA
    | MOV
    | NEG
    | OR
    | POP
    | PUSH
    | RET
    | SBB
    | SETE
    | SETG
    | SETGE
    | SETL
    | SETLE
    | SETNE
    | SHL
    | SHR
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


instance Display Command where
    display cmd = map toLower $ show cmd


-- Pseudo-commands that render data rather than x64 instructions
dataCommands = [DB, DW, DD, DQ]


-- Jump/call commands whose operands reference RIP-relative addresses
jumpCommands = [CALL, JE, JG, JMP]


data Size =
      BYTE
    | WORD
    | DWORD
    | QWORD
    | NoSize
    deriving (Eq, Show)


instance Display Size where
    display NoSize = ""
    display s = map toLower $ show s


readSize :: String -> Size
readSize str
    | s == "BYTE"  = BYTE
    | s == "WORD"  = WORD
    | s == "DWORD" = DWORD
    | s == "QWORD" = QWORD
    | otherwise    = NoSize
    where s = map toUpper str


dataCommandSize :: Command -> Size
dataCommandSize DB = BYTE
dataCommandSize DW = WORD
dataCommandSize DD = DWORD
dataCommandSize DQ = QWORD


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


data Registers =
      NoRegister
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


instance Display Registers where
    display r = map toLower $ show r


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


uniformByteRegisters = [SPL, BPL, SIL, DIL]


registers64 = [RAX, RBX, RCX, RDX, RBP, RSP, RSI, RDI, RIP] ++
              extendedRegisters


data Displacement =
      NoDisplacement
    | Displacement8       Int8
    | Displacement32      Int32
    | DisplacementSymbol  Size  String
    deriving Show


instance Display Displacement where
    display NoDisplacement           = ""
    display (DisplacementSymbol _ s) = "+ " ++ s
    display (Displacement8 d)
        | d < 0     = "- " ++ show (-1 * d)
        | otherwise = "+ " ++ show d
    display (Displacement32 d)
        | d < 0     = "- " ++ show (-1 * d)
        | otherwise = "+ " ++ show d


data Scale =
      NoScale
    | Scale2
    | Scale4
    | Scale8
    deriving Show


instance Display Scale where
    display NoScale = ""
    display Scale2  = "2 *"
    display Scale4  = "4 *"
    display Scale8  = "8 *"


data ImmediateDescriptor =
      Literal [Word8]
    | Symbol  Size    String
    deriving Show


instance Display ImmediateDescriptor where
    display (Literal bytes) = intercalate ", " $ map show bytes
    display (Symbol _ s)    = s


data Operand =
      Register   Registers
    | Address    Registers Scale Registers Displacement
    | Immediate  ImmediateDescriptor
    | Relative   ImmediateDescriptor
    deriving Show


instance Display Operand where
    display (Register r)      = display r
    display (Address b s i d) = "[" ++ display b ++ " + " ++ display s ++
                                display i ++ display d ++ "]"
    display (Immediate i)     = display i
    display (Relative i)      = display i


data Instruction = Instruction {
    source     :: String,
    labelNames :: [String],
    sizeHint   :: Size,
    command    :: Command,
    operands   :: [Operand]
}   deriving Show


instance Display Instruction where
    display inst = labels ++ "    " ++ display (command inst) ++ " " ++ opers
        where labels = concat (map (++ ":\n") $ labelNames inst)
              opers  = intercalate ", " $ map display $ operands inst


data Section = Section {
    sectionName  :: String,
    instructions :: [Instruction]
}   deriving Show


instance Display Section where
    display sec = "section " ++ sectionName sec ++ "\n" ++
                  (intercalate "\n" $ map display $ instructions sec)


-- A description of the possible values of an operand, based on ref.x86asm.net
data Pattern =
      P_r8            -- An 8-bit register
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


instance Show Encoding where
    show enc = show (mnemonic enc) ++ " " ++ show (primary enc)


-- Symbol table entry type (part of st_info)
data SymbolType =
      STT_NOTYPE
    | STT_SECTION
    | STT_FILE
    | STT_FUNC
    deriving (Eq, Show)


instance Display SymbolType where
    display STT_NOTYPE  = ""
    display STT_SECTION = "section"
    display STT_FILE    = "file"
    display STT_FUNC    = "function"


instance Read SymbolType where
    readsPrec _ ('f':'u':'n':'c':'t':'i':'o':'n':rem) = [(STT_FUNC, rem)]
    readsPrec _ _ = []


-- Get the size/width of a register
registerSize :: Registers -> Size
registerSize r
    | elem r registers8  = BYTE
    | elem r registers16 = WORD
    | elem r registers64 = QWORD
    | otherwise          = DWORD


data Directive =
      Global SymbolType String
    | Extern String
    deriving (Eq, Show)


instance Display Directive where
    display (Global STT_NOTYPE s) = "global " ++ s
    display (Global t          s) = "global " ++ s ++ ":" ++ display t
    display (Extern            s) = "extern " ++ s
