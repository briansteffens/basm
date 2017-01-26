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
import Encodings


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
    encoding     :: Encoding,
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
encodeRex :: Encoding -> Size -> [Operand] -> [Word8]
encodeRex _ _ [] = []
encodeRex enc size [op1, op2]
    | (size == QWORD && default32 enc) ||
      anyExtendedRegisters op1 ||
      anyExtendedRegisters op2 = [bitsToByte [0, 1, 0, 0,
                                              if size == QWORD then 1 else 0,
                                              extensionR [op1, op2],
                                              extensionX [op1, op2],
                                              extensionB [op1, op2]]]
    | otherwise                = []


-- Encode a size override prefix if necessary.
encodeSizePrefix :: Encoding -> Size -> [Word8]
encodeSizePrefix enc size
    | default32 enc && size == WORD = [0x66]
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
encodedOrder :: Encoding -> [Operand] -> [Operand]
encodedOrder enc operands
    | reverseOpers enc = reverse operands
    | otherwise        = operands


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
encodeModRm :: Encoding -> [Operand] -> [Word8]
encodeModRm enc [op1, op2]
    | registerAdd enc = []
    | otherwise       = [bitsToByte (modBits [op1, op2] ++
                                     regBits op2 ++
                                     rmBits op1)]
encodeModRm _ _       = []


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


-- Get the register from the first operand or fail.
extractFirstOperand :: [Operand] -> Registers
extractFirstOperand [Register r, _] = r
extractFirstOperand _               = error("Expected a register in first " ++
                                            "operand.")


-- Add register index to a base opcode (for example MOV b0+r / b8+r).
addRegister :: Word8 -> Registers -> Word8
addRegister base register = do
    let bits = registerIndex register
    let adjustment = shiftL (bits !! 0) 2 + shiftL (bits !! 1) 1 + bits !! 2
    base + (fromIntegral adjustment :: Word8)


-- Get the primary opcode, making any necessary adjustments (registerAdd).
resolveOpCode :: Encoding -> Instruction -> Word8
resolveOpCode enc inst = do
    let code = primary enc
    if registerAdd enc
        then addRegister code (extractFirstOperand (operands inst))
        else code


-- Get the data from an immediate literal operand or fail.
extractLiteral :: Operand -> [Word8]
extractLiteral (Immediate (Literal b)) = b
extractLiteral _                       = error("Invalid operand for data " ++
                                               "command.")


-- Get a list of byte counts a given immediate can contain.
immBytes :: Pattern -> [Int]
immBytes P_imm8      = [1]
immBytes P_imm1632   = [2, 4]
immBytes P_imm163264 = [2, 4, 8]
immBytes _           = []


-- Check if a register is 8-bit.
is8 :: Registers -> Bool
is8 r = elem r registers8


-- Check if an operand can match the given pattern.
matchPattern :: Operand -> Pattern -> Bool

matchPattern (Register r) (R r2)          = r == r2

matchPattern (Register r) P_r8            = elem r registers8
matchPattern (Register r) P_rm8           = elem r registers8
matchPattern (Register r) P_r163264       = not (elem r registers8)
matchPattern (Register r) P_rm163264      = not (elem r registers8)

matchPattern (Address _ _ _ _) P_rm8      = True
matchPattern (Address _ _ _ _) P_rm163264 = True

matchPattern (Immediate (Literal l)) p    = elem (length l) (immBytes p)

matchPattern (Immediate (Symbol s _)) p   = elem (sizeInt s) (immBytes p)

matchPattern _ _                          = False


-- Check if an instruction can be encoded using the given Encoding.
matchEncoding :: Instruction -> Encoding -> Bool
matchEncoding inst enc = do
    let opers = (operands inst)
    let pat = (patterns enc)

    all (== True) ([command inst == mnemonic enc,
                    length pat == length opers] ++
                   zipWith matchPattern opers pat)


-- Find all Encodings that can encode the given instruction.
candidates :: Instruction -> [Encoding]
candidates inst = do
    let match enc = case matchEncoding inst enc of True  -> [enc]
                                                   False -> []

    concat (map match encodings)


-- Find a valid Encoding for the given instruction.
chooseEncoding :: Instruction -> Encoding
chooseEncoding inst =
    case candidates inst of
        (first:rest) -> first
        _            -> error("No valid encoding found for instruction")


-- Encode a data pseudo-instruction into bytes (the command must be in
-- dataCommands).
encodeData :: Instruction -> Encoded
encodeData i = do
    Encoded {
        encoding     = dataEncoding,
        sizePrefix   = [],
        rex          = [],
        op           = [],
        modrm        = [],
        sib          = [],
        displacement = [],
        immediate    = concat (map extractLiteral (operands i))
    }


-- Encode a code instruction into bytes (the command can't be in dataCommands).
encodeCode :: Instruction -> Encoded
encodeCode i = do
    let enc = chooseEncoding i
    let pref0f = if prefix0f enc then [0x0f] else []
    let primaryOp = resolveOpCode enc i
    let op = pref0f ++ [primaryOp]
    let ordered = encodedOrder enc (operands i)

    let size = case opSize (sizeHint i) ordered of
              Success   s -> s
              Warn    m s -> trace m s
              Fail    m   -> error m

    Encoded {
        encoding     = enc,
        sizePrefix   = encodeSizePrefix enc size,
        rex          = encodeRex enc size ordered,
        op           = op,
        modrm        = encodeModRm enc ordered,
        sib          = encodeSib ordered,
        displacement = encodeDisplacement ordered,
        immediate    = encodeImmediate ordered
    }


-- Encode an instruction into bytes.
encodeInstruction :: Instruction -> Encoded
encodeInstruction i
    | elem (command i) dataCommands = encodeData i
    | otherwise                     = encodeCode i


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

    let ordered = encodedOrder (encoding enc) (operands inst)

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
