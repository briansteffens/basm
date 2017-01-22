module Encoder where

import Data.Int
import Data.List
import Data.Binary
import Data.Bits
import qualified Data.ByteString as B

import Numeric (showHex)
import Debug.Trace (trace)

import Shared
import Definitions


-- NOTE FOR EDITING THIS FILE:
--
-- There are two operand orders in this file:
--
--     - Assembly order: The default order of operators as they appear in
--                       assembly code and the Instruction structure.
--     - Encoded order:  The same operators reordered to match how the machine
--                       code they will generate.
--
-- Converting from assembly order to encoded order can be done with the
-- function encodedOrder.
--
-- Unless otherwise noted, all functions in this file expect operands to be in
-- encoded order. A notable exception is encodeInstruction, since it is the
-- primary exported/public interface to this file.


data Result a = Success a
              | Warn String a
              | Fail String


data Encoded = Encoded {
    sizePrefix   :: [Word8],
    rex          :: [Word8],
    op           :: [Word8],
    modrm        :: [Word8],
    sib          :: [Word8],
    displacement :: [Word8],
    immediate    :: [Word8]
}


data NamedOffset = NamedOffset {
    name   :: String,
    offset :: Int,
    size   :: Size
}


data Label = Label {
    label       :: String,
    labelOffset :: Int
}


data EncodedSection = EncodedSection {
    section :: CodeSection,
    bytes   :: [Word8],
    labels  :: [Label],
    symbols :: [NamedOffset]
}


-- Opcodes that default to 32-bit operation size without any prefix bytes
ops32 = [
         -- ADD
         [0x01], [0x03], [0x05], [0x81], [0x83],

         -- MOV
         [0x89], [0x8b], [0xc7],
         [0xb8], [0xb9], [0xba], [0xbb], [0xbc], [0xbd], [0xbe], [0xbf]] --b8+r


-- Opcodes that specify one of the operands and therefore need no ModR/M byte
setRegister = [
               -- ADD
               [0x04], [0x05],

               -- MOV
               [0xb0], [0xb1], [0xb2], [0xb3], [0xb4], [0xb5], [0xb6], [0xb7],
               [0xb8], [0xb9], [0xba], [0xbb], [0xbc], [0xbd], [0xbe], [0xbf]]


-- Opcodes in this list need their operators reversed before encoding.
reversedOpCodes = [[0x02], [0x03]]


-- Check if an operand references any of a list of registers
anyRegisters :: Operand -> [Registers] -> Bool
anyRegisters (Immediate _        ) _    = False
anyRegisters (Register  r        ) regs = elem r  regs
anyRegisters (Address   r1 _ r2 _) regs = elem r1 regs || elem r2 regs


-- Check if an operand references any extended registers
anyExtendedRegisters :: Operand -> Bool
anyExtendedRegisters operand = anyRegisters operand extendedRegisters


-- The optional extension bit for encoding a register.
exBit :: Registers -> Int
exBit r = if elem r extendedRegisters then 1 else 0


-- Generate the extension bit of the MODRM.reg field.
extensionR :: [Operand] -> Int
extensionR [_, Register r] = exBit r
extensionR [_, _         ] = 0


-- Generate the extension bit of the SIB.index field.
extensionX :: [Operand] -> Int
extensionX [Address _ _ r _, _] = exBit r
extensionX [_              , _] = 0


-- Generate the extension bit of the MODRM.rm or SIB.base field.
extensionB :: [Operand] -> Int
extensionB [Register r      , _] = exBit r
extensionB [Address  r _ _ _, _] = exBit r
extensionB [_               , _] = 0


-- Encode a REX byte if necessary.
-- TODO: Make sure extended registers and high byte registers aren't mixed
encodeRex :: [Word8] -> Size -> [Operand] -> [Word8]
encodeRex _ _ [] = []
encodeRex op size [op1, op2]
    | (size == QWORD && elem op ops32) ||
      anyExtendedRegisters op1 ||
      anyExtendedRegisters op2 = [bitsToByte [0, 1, 0, 0,
                                              if size == QWORD then 1 else 0,
                                              extensionR [op1, op2],
                                              extensionX [op1, op2],
                                              extensionB [op1, op2]]]
    | otherwise                = []


-- Encode a size override prefix if necessary.
encodeSizePrefix :: [Word8] -> Size -> [Word8]
encodeSizePrefix op size
    | elem op ops32 && size == WORD = [0x66]
    | otherwise                     = []


-- Get the size/width of a register
registerSize :: Registers -> Size
registerSize r
    | elem r registers8  = BYTE
    | elem r registers16 = WORD
    | elem r registers64 = QWORD
    | otherwise          = DWORD


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
registerIndex NoRegister                                    = [0, 0, 0]
registerIndex r
    | elem r [AL, AX, EAX, RAX, R8B, R8W, R8D, R8]          = [0, 0, 0]
    | elem r [CL, CX, ECX, RCX, R9B, R9W, R9D, R9]          = [0, 0, 1]
    | elem r [DL, DX, EDX, RDX, R10B, R10W, R10D, R10]      = [0, 1, 0]
    | elem r [BL, BX, EBX, RBX, R11B, R11W, R11D, R11]      = [0, 1, 1]
    | elem r [AH, SPL, SP, ESP, RSP, R12B, R12W, R12D, R12] = [1, 0, 0]
    | elem r [CH, BPL, BP, EBP, RBP, R13B, R13W, R13D, R13] = [1, 0, 1]
    | elem r [DH, SIL, SI, ESI, RSI, R14B, R14W, R14D, R14] = [1, 1, 0]
    | elem r [BH, DIL, DI, EDI, RDI, R15B, R15W, R15D, R15] = [1, 1, 1]


-- Reorder operands based on opcode directionality (flipping them if needed).
encodedOrder :: [Word8] -> [Operand] -> [Operand]
encodedOrder op operands
    | elem op reversedOpCodes = reverse operands
    | otherwise               = operands


-- Generate the first two bits of ModR/M field.
modBits :: [Operand] -> [Int]
modBits [Address  _   _ _  NoDisplacement         , _] = [0, 0]
modBits [Address  RIP _ _ (Displacement32     _  ), _] = [0, 0]
modBits [Address  RIP _ _ (DisplacementSymbol _ _), _] = [0, 0]
modBits [Address  EIP _ _ (Displacement32     _  ), _] = [0, 0]
modBits [Address  EIP _ _ (DisplacementSymbol _ _), _] = [0, 0]
modBits [Address  _   _ _ (Displacement8      _  ), _] = [0, 1]
modBits [Address  _   _ _ (Displacement32     _  ), _] = [1, 0]
modBits [Address  _   _ _ (DisplacementSymbol _ _), _] = [1, 0]
modBits [Register _                               , _] = [1, 1]


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
encodeModRm :: [Word8] -> [Operand] -> [Word8]
encodeModRm _  []         = []
encodeModRm op [op1, op2]
    | elem op setRegister = []
    | otherwise           = [bitsToByte (modBits [op1, op2] ++
                                         regBits op2 ++
                                         rmBits op1)]


-- Convert a scale into its 2-bit SIB byte representation.
encodeScale :: Scale -> [Int]
encodeScale NoScale = [0, 0]
encodeScale Scale2  = [0, 1]
encodeScale Scale4  = [1, 0]
encodeScale Scale8  = [1, 1]


-- Encode a SIB byte if necessary.
encodeSib :: [Operand] -> [Word8]
encodeSib [Address _    NoScale NoRegister _, _] = []
encodeSib [Address base scale   index      _, _] =
    [bitsToByte (encodeScale scale ++
                 registerIndex index ++
                 registerIndex base)]
encodeSib [_, _]                                 = []
encodeSib []                                     = []


-- Encode a displacement if one exists.
encodeDisplacement :: [Operand] -> [Word8]
encodeDisplacement [Address _ _ _ (Displacement8        d), _] = toBytes d
encodeDisplacement [Address _ _ _ (Displacement32       d), _] = toBytes d
encodeDisplacement [Address _ _ _ (DisplacementSymbol s _), _] =
    replicate (sizeInt s) 0
encodeDisplacement _                                           = []


-- Encode an immediate if one exists.
encodeImmediate :: [Operand] -> [Word8]
encodeImmediate [_, Immediate (Literal imm  )] = imm
encodeImmediate [_, Immediate (Symbol  s   _)] = replicate (sizeInt s) 0
encodeImmediate _                              = []


-- A description of the possible values of an operand, based on ref.x86asm.net
data Pattern = P_empty      -- An empty operand with no valid values
             | P_r8         -- An 8-bit register
             | P_rm8        -- An 8-bit register or memory address
             | P_r163264    -- A 16/32/64-bit register
             | P_rm163264   -- A 16/32/64-bit register or memory address
             | P_imm8       -- An 8-bit immediate
             | P_imm1632    -- A 16/32-bit immediate
             | P_imm163264  -- A 16/32/64-bit immediate
             | P Registers  -- A single register match
             deriving Show


-- These instructions all share the same combinations of possible operands.
standardIntCommands = [ADC, ADD, AND, CMP, OR, SBB, SUB, XOR]


-- These instructions have no operands (or all operands are implicit).
noOperandCommands = [SYSCALL]


-- Operand classifier for the standard integer commands (standardIntCommands).
-- Operands should not be in encoded order.
patStdInt :: [Operand] -> [Pattern]
patStdInt [_,               Register r]
    | elem r registers8                              = [P_rm8     , P_r8      ]
    | otherwise                                      = [P_rm163264, P_r163264 ]
patStdInt [Register r,      Address _ _ _ _]
    | elem r registers8                              = [P_r8      , P_rm8     ]
    | otherwise                                      = [P_r163264 , P_rm163264]
patStdInt [Register AL,     Immediate _]             = [P AL      , P_imm8    ]
patStdInt [Register AX,     Immediate _]             = [P RAX     , P_imm1632 ]
patStdInt [Register EAX,    Immediate _]             = [P RAX     , P_imm1632 ]
patStdInt [Register RAX,    Immediate _]             = [P RAX     , P_imm1632 ]
patStdInt [Address _ _ _ _, Immediate (Literal [_])] = [P_rm8     , P_imm8    ]
patStdInt [Register r,      Immediate (Literal [_])]
    | elem r registers8                              = [P_rm8     , P_imm8    ]
    | otherwise                                      = [P_rm163264, P_imm8    ]
patStdInt [_,               Immediate _]             = [P_rm163264, P_imm1632 ]


-- Operand classifier for the mov instruction.
-- Operands should not be in encoded order.
patMov :: [Operand] -> [Pattern]
patMov [_,               Register r]
    | elem r registers8                           = [P_rm8     , P_r8       ]
    | otherwise                                   = [P_rm163264, P_r163264  ]
patMov [Register r,      Address _ _ _ _]
    | elem r registers8                           = [P_r8      , P_rm8      ]
    | otherwise                                   = [P_r163264 , P_rm163264 ]
patMov [Register r,      Immediate _]
    | elem r registers8                           = [P r       , P_imm8     ]
    | otherwise                                   = [P r       , P_imm163264]
patMov [Address _ _ _ _, Immediate (Literal [_])] = [P_rm8     , P_imm8     ]
patMov [Address _ _ _ _, Immediate _]             = [P_rm163264, P_imm1632  ]


-- Classify a set of operands into an operand pattern.
-- Operands should not be in encoded order.
pattern :: Command -> [Operand] -> [Pattern]
pattern cmd operands
    | cmd == MOV                   = patMov    operands
    | elem cmd standardIntCommands = patStdInt operands
    | elem cmd noOperandCommands   = []


-- Add register index to a base opcode (for example MOV b0+r / b8+r)
addRegister :: Word8 -> Registers -> Word8
addRegister base register = do
    let bits = registerIndex register
    let adjustment = shiftL (bits !! 0) 2 + shiftL (bits !! 1) 1 + bits !! 2
    base + (fromIntegral adjustment :: Word8)


-- Get an opcode based on an assembly command and an operator pattern.
opcode :: Command -> [Pattern] -> [Word8]

opcode ADD     [P_rm8     , P_r8       ] = [      0x00]
opcode ADD     [P_rm163264, P_r163264  ] = [      0x01]
opcode ADD     [P_r8      , P_rm8      ] = [      0x02]
opcode ADD     [P_r163264 , P_rm163264 ] = [      0x03]
opcode ADD     [P AL      , P_imm8     ] = [      0x04]
opcode ADD     [P RAX     , P_imm1632  ] = [      0x05]
opcode ADD     [P_rm8     , P_imm8     ] = [      0x80]
opcode ADD     [P_rm163264, P_imm1632  ] = [      0x81]
opcode ADD     [P_rm163264, P_imm8     ] = [      0x83]

opcode MOV     [P_rm8     , P_r8       ] = [      0x88]
opcode MOV     [P_rm163264, P_r163264  ] = [      0x89]
opcode MOV     [P_r8      , P_rm8      ] = [      0x8a]
opcode MOV     [P_r163264 , P_rm163264 ] = [      0x8b]
opcode MOV     [P r       , P_imm8     ] = [addRegister 0xb0 r]
opcode MOV     [P r       , P_imm163264] = [addRegister 0xb8 r]
opcode MOV     [P_rm8     , P_imm8     ] = [      0xc6]
opcode MOV     [P_r163264 , P_imm1632  ] = [      0xc7]

opcode SYSCALL [                       ] = [0x0f, 0x05]


-- Encode an instruction into bytes.
encodeInstruction :: Instruction -> Encoded
encodeInstruction i = do
    let op = opcode (command i) (pattern (command i) (operands i))
    let ordered = encodedOrder op (operands i)

    let size = case opSize (sizeHint i) ordered of
              Success   s -> s
              Warn    m s -> trace m s
              Fail    m   -> error m

    Encoded {
        sizePrefix   = encodeSizePrefix op size,
        rex          = encodeRex op size ordered,
        op           = op,
        modrm        = encodeModRm op ordered,
        sib          = encodeSib ordered,
        displacement = encodeDisplacement ordered,
        immediate    = encodeImmediate ordered
    }


-- Get the total number of encoded bytes for an instruction.
encodedLength :: Encoded -> Int
encodedLength e = length (sizePrefix   e) +
                  length (rex          e) +
                  length (op           e) +
                  length (modrm        e) +
                  length (sib          e) +
                  length (displacement e) +
                  length (immediate    e)


-- Extract encoded bytes from an instruction.
encodedBytes :: Encoded -> [Word8]
encodedBytes e = (sizePrefix   e) ++
                 (rex          e) ++
                 (op           e) ++
                 (modrm        e) ++
                 (sib          e) ++
                 (displacement e) ++
                 (immediate    e)


-- If a displacement symbol is present, return the name and size.
displacementSymbol :: [Operand] -> Maybe (Size, String)
displacementSymbol [Address _ _ _ (DisplacementSymbol s n), _] = Just (s, n)
displacementSymbol _                                           = Nothing


-- If an immediate symbol is present, return the name and size.
immediateSymbol :: [Operand] -> Maybe (Size, String)
immediateSymbol [_, Immediate (Symbol s n)] = Just (s, n)
immediateSymbol _                           = Nothing


-- Extract any symbol offsets from an encoded instruction.
symbolOffsets :: Instruction -> Encoded -> Int -> [NamedOffset]
symbolOffsets inst enc offset = do
    let commandByteLen = offset +
                         (length (sizePrefix enc)) +
                         (length (rex enc)) +
                         (length (op enc)) +
                         (length (modrm enc)) +
                         (length (sib enc))

    let ordered = encodedOrder (op enc) (operands inst)

    let make s n o = NamedOffset { name = n, offset = o, size = s }

    let disp = case displacementSymbol ordered of
              Just (s, n) -> [make s n commandByteLen]
              Nothing     -> []

    let imm = case immediateSymbol ordered of
             Just (s, n) -> [make s n (commandByteLen +
                                      (length (displacement enc)))]
             Nothing     -> []

    disp ++ imm


-- Extract all symbol offsets from a list of encoded instructions.
allSymbolOffsets :: [(Instruction, Encoded)] -> Int -> [NamedOffset]
allSymbolOffsets [] _ = []
allSymbolOffsets ((inst, enc):xs) offset = do
    let inner = allSymbolOffsets xs (offset + (encodedLength enc))
    let current = symbolOffsets inst enc offset
    current ++ inner


-- Extract all label offsets from a list of encoded instructions.
allLabelOffsets :: [(Instruction, Encoded)] -> Int -> [Label]
allLabelOffsets [] _ = []
allLabelOffsets ((inst, enc):xs) offsetCurrent = do
    let inner = allLabelOffsets xs (offsetCurrent + (encodedLength enc))
    let make l = Label { label = l, labelOffset = offsetCurrent }
    let current = map make (labelNames inst)
    current ++ inner


-- Encode a section.
encodeSection :: CodeSection -> EncodedSection
encodeSection sec = do
    let encodeOne i = (i, encodeInstruction i)
    let encoded = map encodeOne (instructions sec)

    let bytes (_, e) = encodedBytes e

    EncodedSection {
        section = sec,
        bytes   = concat (map bytes encoded),
        symbols = allSymbolOffsets encoded 0,
        labels  = allLabelOffsets encoded 0
    }
