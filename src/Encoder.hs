module Encoder where

import Data.Int
import Data.List
import Data.Binary
import Data.Bits
import qualified Data.ByteString as B
import Data.Maybe

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


instance Show Encoded where
    show enc = intercalate "\n" [
        "Encoded {",
        "    encoding     = " ++ show (encoding enc),
        "    sizePrefix   = " ++ show (sizePrefix enc),
        "    rex          = " ++ show (rex enc),
        "    op           = " ++ show (op enc),
        "    modrm        = " ++ show (modrm enc),
        "    sib          = " ++ show (sib enc),
        "    displacement = " ++ show (displacement enc),
        "    immediate    = " ++ show (immediate enc),
        "}"]


data OffsetType = OffsetImmediate
                | OffsetDisplacement
                deriving Eq


data NamedOffset = NamedOffset {
    name       :: String,
    offset     :: Int,
    size       :: Size,
    offsetType :: OffsetType
}


data Label = Label {
    label       :: String,
    labelOffset :: Int
}


data EncodedSection = EncodedSection {
    section :: Section,
    bytes   :: [Word8],
    labels  :: [Label],
    symbols :: [NamedOffset]
}


-- Check if an operand references any of a list of registers
anyRegisters :: Operand -> [Registers] -> Bool
anyRegisters (Immediate _        ) _    = False
anyRegisters (Register  r        ) regs = elem r  regs
anyRegisters (Address   r1 _ r2 _) regs = elem r1 regs || elem r2 regs
anyRegisters (Relative  _        ) _    = False


-- Check if an operand references any extended registers
anyExtendedRegisters :: Operand -> Bool
anyExtendedRegisters operand = anyRegisters operand extendedRegisters


-- Check if an operand references any uniform byte registers
anyUniformByteRegisters :: Operand -> Bool
anyUniformByteRegisters operand = anyRegisters operand uniformByteRegisters


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
-- TODO: Unify the 1- and 2-operand versions below
encodeRex :: Encoding -> Size -> [Operand] -> [Word8]
encodeRex _ _ []  = []
encodeRex enc size [op]
    | (size == QWORD && default32 enc) ||
      anyUniformByteRegisters op ||
      anyExtendedRegisters op = [bitsToByte [0, 1, 0, 0,
                                             if size == QWORD then 1 else 0,
                                             extensionR [op, op],
                                             extensionX [op, op],
                                             extensionB [op, op]]]
    | otherwise               = []
encodeRex enc size [op1, op2]
    | (size == QWORD && default32 enc) ||
      anyUniformByteRegisters op1 ||
      anyUniformByteRegisters op2 ||
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


-- Infer the operation size based on its operands, if possible.
inferOpSize :: [Operand] -> Result Size
inferOpSize [Immediate _                     ] = Success QWORD
inferOpSize [Relative (Symbol s _)           ] = Success s
inferOpSize [Relative (Literal l)            ] = Success (intSize (length l))
inferOpSize [Register r                      ] = Success (registerSize r)
inferOpSize [Address _ _ _ _, Immediate _    ] = Success NoSize
inferOpSize [Register r     , Immediate _    ] = Success (registerSize r)
inferOpSize [Address _ _ _ _, Register r     ] = Success (registerSize r)
inferOpSize [Register r     , Address _ _ _ _] = Success (registerSize r)
inferOpSize [Register r1    , Register r2    ]
    | size1 == size2                           = Success size1
    | otherwise                                = Fail ("Operand size " ++
                                                          "mismatch")
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


-- Convert a 3-bit integer into a list of bits.
-- TODO: find a built-in?
toBits :: Word8 -> [Int]
toBits 0 = [0, 0, 0]
toBits 1 = [0, 0, 1]
toBits 2 = [0, 1, 0]
toBits 3 = [0, 1, 1]
toBits 4 = [1, 0, 0]
toBits 5 = [1, 0, 1]
toBits 6 = [1, 1, 0]
toBits 7 = [1, 1, 1]
toBits n = error("Invalid toBits argument: " ++ show n)


-- Get the least significant 3 bits for a register.
registerIndex :: Registers -> [Int]
registerIndex NoRegister                                    = toBits 0
registerIndex r
    | elem r [AL, AX, EAX, RAX, R8B, R8W, R8D, R8]          = toBits 0
    | elem r [CL, CX, ECX, RCX, R9B, R9W, R9D, R9]          = toBits 1
    | elem r [DL, DX, EDX, RDX, R10B, R10W, R10D, R10]      = toBits 2
    | elem r [BL, BX, EBX, RBX, R11B, R11W, R11D, R11]      = toBits 3
    | elem r [AH, SPL, SP, ESP, RSP, R12B, R12W, R12D, R12] = toBits 4
    | elem r [CH, BPL, BP, EBP, RBP, R13B, R13W, R13D, R13] = toBits 5
    | elem r [DH, SIL, SI, ESI, RSI, R14B, R14W, R14D, R14] = toBits 6
    | elem r [BH, DIL, DI, EDI, RDI, R15B, R15W, R15D, R15] = toBits 7


-- Reorder operands based on opcode directionality (flipping them if needed).
encodedOrder :: Encoding -> [Operand] -> [Operand]
encodedOrder enc operands
    | reverseOpers enc = reverse operands
    | otherwise        = operands


-- Generate the first two bits of ModR/M field.
modBits :: [Operand] -> [Int]
modBits [Address  _          _ _  NoDisplacement         , _] = [0, 0]
modBits [Address  RIP        _ _ (Displacement32     _  ), _] = [0, 0]
modBits [Address  RIP        _ _ (DisplacementSymbol _ _), _] = [0, 0]
modBits [Address  EIP        _ _ (Displacement32     _  ), _] = [0, 0]
modBits [Address  EIP        _ _ (DisplacementSymbol _ _), _] = [0, 0]
modBits [Address  NoRegister _ _ (DisplacementSymbol _ _), _] = [0, 0]
modBits [Address  _          _ _ (Displacement8      _  ), _] = [0, 1]
modBits [Address  _          _ _ (Displacement32     _  ), _] = [1, 0]
modBits [Address  _          _ _ (DisplacementSymbol _ _), _] = [1, 0]
modBits [Register _                                      , _] = [1, 1]
modBits [Register _                                         ] = [1, 1]


-- Generate the R/M bits of a ModR/M byte.
rmBits :: Operand -> [Int]
rmBits (Register r                                        ) = registerIndex r
rmBits (Address  RIP NoScale NoRegister (Displacement32 _)) = [1, 0, 1]
rmBits (Address  EIP NoScale NoRegister (Displacement32 _)) = [1, 0, 1]
rmBits (Address NoRegister NoScale NoRegister (DisplacementSymbol _ _)) =
    [1, 0, 0] -- SIB
rmBits (Address  r   NoScale NoRegister _                 ) = registerIndex r
rmBits (Address  _   _       _          _                 ) = [1, 0, 0] -- SIB


-- Generate the REG bits of a ModR/M byte.
regBits :: Encoding -> Operand -> [Int]
regBits Encoding { opExtension=Just ext } _ = toBits ext
regBits enc (Register r)                    = registerIndex r
regBits _ _                                 = [0, 0, 0]


-- Generate a ModR/M byte if necessary.
-- TODO: unify the 1- and 2-operand versions below
encodeModRm :: Encoding -> [Operand] -> [Word8]
encodeModRm Encoding{opExtension=Just ext} [op] =
    [bitsToByte (modBits [op] ++ toBits ext ++ rmBits op)]
encodeModRm enc [op]  = [bitsToByte (modBits [op] ++ [0, 0, 0] ++ rmBits op)]
encodeModRm enc [op1, op2]
    | registerAdd enc = []
    | otherwise       = [bitsToByte (modBits [op1, op2] ++
                                     regBits enc op2 ++
                                     rmBits op1)]
encodeModRm _ _       = []


-- Convert a scale into its 2-bit SIB byte representation.
encodeScale :: Scale -> [Int]
encodeScale NoScale = [0, 0]
encodeScale Scale2  = [0, 1]
encodeScale Scale4  = [1, 0]
encodeScale Scale8  = [1, 1]


encodeSibIndex :: Registers -> [Int]
encodeSibIndex NoRegister = toBits 4
encodeSibIndex r
    | elem r [AH, SPL, SP, ESP, RSP, R12B, R12W, R12D, R12] =
        error(show r ++ " cannot be an index register")
    | otherwise = registerIndex r


-- Encode a SIB byte if necessary.
encodeSib :: [Operand] -> [Word8]
encodeSib [Address base scale index (DisplacementSymbol _ _), _] =
    [bitsToByte (encodeScale scale ++
                 encodeSibIndex index ++
                 [1, 0, 1])]
encodeSib [Address R12  scale   index      _, _] =
    [bitsToByte (encodeScale scale ++
                 encodeSibIndex index ++
                 registerIndex R12)]
encodeSib [Address _    NoScale NoRegister _, _] = []
encodeSib [Address base scale   index      _, _] =
    [bitsToByte (encodeScale scale ++
                 encodeSibIndex index ++
                 registerIndex base)]
encodeSib [_, _]                                 = []
encodeSib [_]                                    = []
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
encodeImmediate [Relative (Literal imm  )]     = imm
-- TODO: remove when encodedLength is fixed
encodeImmediate [Relative (Symbol  s   _)]     = replicate (sizeInt s) 0
encodeImmediate [Immediate (Literal imm)]      = imm
encodeImmediate _                              = []


-- Get the register from the first operand or fail.
extractFirstOperand :: [Operand] -> Registers
extractFirstOperand [Register r, _] = r
extractFirstOperand [Register r   ] = r
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
immBytes P_rel8      = [1]
immBytes P_rel1632   = [2, 4]
immBytes _           = []


-- Check if a number of bytes is small enough to fit in any variations of an
-- immediate pattern.
immFit :: Int -> Pattern -> Bool
immFit size pattern = any (>= size) (immBytes pattern)


-- Check if a register is 8-bit.
is8 :: Registers -> Bool
is8 r = elem r registers8


-- Get the byte/word size of an instruction under a given encoding
instructionSize :: Instruction -> Encoding -> Size
instructionSize inst enc = do
    let ordered = encodedOrder enc (operands inst)

    case opSize (sizeHint inst) ordered of
        Success   s -> s
        Warn    m s -> trace m s
        Fail    m   -> error m


-- Check if an operand can match the given pattern.
matchPattern :: Operand -> Pattern -> Bool

matchPattern (Register r) (R r2)          = r == r2

matchPattern (Register r) P_r8            = elem r registers8
matchPattern (Register r) P_rm8           = elem r registers8
matchPattern (Register r) P_r163264       = not (elem r registers8)
matchPattern (Register r) P_rm163264      = not (elem r registers8)
matchPattern (Register r) P_r6416         = elem r registers16 ||
                                            elem r registers64
matchPattern (Register r) P_rm6416        = elem r registers16 ||
                                            elem r registers64

matchPattern (Address _ _ _ _) P_rm8      = True
matchPattern (Address _ _ _ _) P_rm163264 = True
matchPattern (Address _ _ _ _) P_rm6416   = True
matchPattern (Address _ _ _ _) P_m        = True

matchPattern (Immediate (Literal l)) p    = immFit (length l) p
matchPattern (Immediate (Symbol s _)) p   = immFit (sizeInt s) p

matchPattern (Relative  (Literal l)) p    = immFit (length l) p
matchPattern (Relative  (Symbol s _)) p   = immFit (sizeInt s) p

matchPattern _ _                          = False


-- Check if an instruction can be encoded using the given Encoding.
matchEncoding :: Instruction -> Encoding -> Bool
matchEncoding inst enc = do
    let opers = (operands inst)
    let pat = (patterns enc)
    let size = instructionSize inst enc

    let sizeCheck = if null (sizes enc) then [] else [size `elem` (sizes enc)]

    all (== True) ([command inst == mnemonic enc,
                    length pat == length opers] ++
                   sizeCheck ++
                   zipWith matchPattern opers pat)


-- Find all Encodings that can encode the given instruction.
candidates :: Instruction -> [Encoding]
candidates inst = do
    let match enc = case matchEncoding inst enc of True  -> [enc]
                                                   False -> []

    let codeEncodings = concat (map match encodings)

    let isData = elem (command inst) dataCommands

    let dataRet = dataEncoding {
        mnemonic = command inst
    }

    if isData then [dataRet] else codeEncodings


-- Find a valid Encoding for the given instruction.
chooseEncoding :: Instruction -> Encoding
chooseEncoding inst =
    case candidates inst of
        (first:rest) -> first
        _            -> error("No valid encoding found for instruction")


-- Encode a data pseudo-instruction into bytes (the command must be in
-- dataCommands).
encodeData :: Instruction -> Encoding -> Encoded
encodeData i enc = do
    let rawImm = concat (map extractLiteral (operands i))
    let imm = padZeroes rawImm (sizeInt (dataCommandSize (mnemonic enc)))

    Encoded {
        encoding     = enc,
        sizePrefix   = [],
        rex          = [],
        op           = [],
        modrm        = [],
        sib          = [],
        displacement = [],
        immediate    = imm
    }


padZeroes :: [Word8] -> Int -> [Word8]
padZeroes bytes size = bytes ++ replicate (size - length bytes) 0x00


padLiteral :: [Word8] -> Pattern -> Size -> [Word8]
padLiteral bytes pattern size = do
    let maxPatternSize = maximum (immBytes pattern)
    let target = minimum [sizeInt size, maxPatternSize]
    padZeroes bytes target


-- Literals are parsed as the smallest number of bytes which can contain the
-- value. During encoding they need to be padded with zero bytes to the size
-- of the instruction, with a ceiling of the max size of the pattern the
-- literal operand matched with.
padLiterals :: [(Operand, Pattern)] -> Size -> [Operand]
padLiterals [(left, _), (Immediate (Literal b), p)] size = do
    [left, Immediate (Literal (padLiteral b p size))]
padLiterals [           (Immediate (Literal b), p)] size = do
    [Immediate (Literal (padLiteral b p size))]
padLiterals o _ = map fst o


-- Encode a code instruction into bytes (the command can't be in dataCommands).
encodeCode :: Instruction -> Encoding -> Encoded
encodeCode i enc = do
    let pref0f = if prefix0f enc then [0x0f] else []
    let primaryOp = resolveOpCode enc i
    let op = pref0f ++ [primaryOp]
    let size = instructionSize i enc
    let padded = padLiterals (zip (operands i) (patterns enc)) size
    let ordered = encodedOrder enc padded

    Encoded {
        encoding     = enc,
        sizePrefix   = encodeSizePrefix enc size,
        rex          = encodeRex enc size ordered,
        op           = op,
        modrm        = if modRmByte enc then encodeModRm enc ordered else [],
        sib          = encodeSib ordered,
        displacement = encodeDisplacement ordered,
        immediate    = encodeImmediate ordered
    }


-- Encode an instruction into bytes.
encodeInstruction :: Instruction -> Encoding -> Encoded
encodeInstruction i enc
    | elem (mnemonic enc) dataCommands = encodeData i enc
    | otherwise                        = encodeCode i enc


-- Get the total number of encoded bytes for an instruction.
encodedLength :: Encoded -> Int
encodedLength e
    | cmd == DB = length (immediate    e)
    | cmd == DW = sizeInt WORD
    | cmd == DD = sizeInt DWORD
    | cmd == DQ = sizeInt QWORD
    | otherwise = length (sizePrefix   e) +
                  length (rex          e) +
                  length (op           e) +
                  length (modrm        e) +
                  length (sib          e) +
                  length (displacement e) +
                  length (immediate    e)
    where cmd = mnemonic (encoding e)


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
immediateSymbol [Relative (Symbol s n)    ] = Just (s, n)
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

    let make s n o t = NamedOffset { name = n, offset = o, size = s,
                                     offsetType = t }

    let disp = case displacementSymbol ordered of
              Just (s, n) -> [make s n commandByteLen OffsetDisplacement]
              Nothing     -> []

    let imm = case immediateSymbol ordered of
             Just (s, n) -> [make s n (commandByteLen +
                                      (length (displacement enc)))
                                  OffsetImmediate]
             Nothing     -> []

    disp ++ imm


-- Extract all symbol offsets from a list of encoded instructions.
allSymbolOffsets :: [(Instruction, Encoded)] -> Int -> [NamedOffset]
allSymbolOffsets [] _ = []
allSymbolOffsets ((inst, enc):xs) offset = do
    let inner = allSymbolOffsets xs (offset + (encodedLength enc))
    let current = symbolOffsets inst enc offset
    current ++ inner


-- Extract all label offsets from a list of instructions and their encodings.
allLabelOffsets :: [(Instruction, Encoding)] -> Int -> [Label]
allLabelOffsets [] _ = []
allLabelOffsets ((inst, enc):xs) offsetCurrent = do
    let inner = allLabelOffsets xs (offsetCurrent + (encodingLength inst enc))
    let make l = Label { label = l, labelOffset = offsetCurrent }
    let current = map make (labelNames inst)
    current ++ inner


-- Figure out how many bytes an instruction will be encoded as.
-- TODO: Actually calculate this instead of just encoding it and checking.
encodingLength :: Instruction -> Encoding -> Int
encodingLength inst enc = encodedLength (encodeInstruction inst enc)


-- Find a label by name
findLabel :: [Label] -> String -> Maybe Label
findLabel labels search = find (\l -> label l == search) labels


-- Find a label by name which should exist (or panic if not)
findLabelExpect :: [Label] -> String -> Label
findLabelExpect labels search = fromJust (findLabel labels search)


-- Replace a relative symbol with a relative offset (hooks up a jump).
relativeOffset :: Instruction -> Int -> [Label] -> Instruction
relativeOffset inst@Instruction { operands=[Relative (Symbol size str)] }
               offset labels = do
    let label = findLabel labels str

    let rewritten = inst {
        operands = [Relative (Literal (toBytes (fromIntegral
                    ((labelOffset (fromJust label)) - offset) :: Int32)))]
    }

    case label of
        Nothing -> inst
        Just _  -> rewritten
relativeOffset inst _ _ = inst


-- Calculate relative offsets for jumps/calls.
relativeOffsets :: [(Instruction, Encoding)] -> [Label] -> Int ->
                   [(Instruction, Encoding)]
relativeOffsets [] _ _ = []
relativeOffsets ((inst, enc):rest) labels offset = do
    -- Offset of next instruction (RIP value)
    let rip = offset + encodingLength inst enc

    let newInst = relativeOffset inst rip labels

    let inner = relativeOffsets rest labels rip
    [(newInst, enc)] ++ inner


-- x86_64 doesn't allow all combinations of opcodes and alternatives must be
-- chosen in some cases.
exceptions :: Instruction -> Instruction

-- No ModR/M encoding exists for the second operand being R13 with no
-- displacement. The closest valid encoding is R13 with a 0 displacement.
exceptions
    inst@Instruction {
        operands = [
            first,
            Address R13 NoScale NoRegister NoDisplacement
        ]
    } = inst {
        operands = [
            first,
            Address R13 NoScale NoRegister (Displacement8 0)
        ]
    }

exceptions inst = inst


-- Encode a section.
encodeSection :: Section -> EncodedSection
encodeSection sec = do
    let insts = map exceptions (instructions sec)

    -- [Instruction] -> [(Instruction, Encoding)]
    let chooseOne i = (i, chooseEncoding i)
    let instEncodings = map chooseOne insts

    let labels = allLabelOffsets instEncodings 0

    let instEncodings2 = relativeOffsets instEncodings labels 0

    -- [(Instruction, Encoding)] -> [(Instruction, Encoded)]
    let encodeOne (i, e) = (i, encodeInstruction i e)
    let encoded = map encodeOne instEncodings2

    -- [(Instruction, Encoded)] -> [Word8]
    let extractBytes (_, e) = encodedBytes e
    let bytes = concat (map extractBytes encoded)

    EncodedSection {
        section = sec,
        bytes   = bytes,
        symbols = allSymbolOffsets encoded 0,
        labels  = labels
    }
