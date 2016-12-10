module Main where

import System.Environment
import Data.Int
import Data.List
import Data.Char
import Data.Bits
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL

import Parser

calculateMovOffsets :: Instruction -> Int -> Instruction
calculateMovOffsets inst offset = do
    let cmdSize = commandSize (command inst)

    inst {
        instructionOffset = offset,
        operands = [(head (operands inst)) {
            operandOffset = offset + cmdSize,
            operandSize   = 0
        }, (last (operands inst)) {
            operandOffset = offset + cmdSize,
            operandSize   = 8
        }]
    }

calculateJmpOffsets :: Instruction -> Int -> Instruction
calculateJmpOffsets inst offset = do
    let cmdSize = commandSize (command inst)

    inst {
        instructionOffset = offset,
        operands = [(head (operands inst)) {
            operandOffset = offset + cmdSize,
            operandSize   = 4
        }]
    }

calculateDbOperandOffsets :: [Operand] -> Int -> [Operand]
calculateDbOperandOffsets [] _ = []
calculateDbOperandOffsets remaining offset = do
    let current = head remaining

    let ret = current {
        operandOffset = offset,
        operandSize = length (renderDbOperand current) -- renderDbOperand hack
    }

    [ret] ++ calculateDbOperandOffsets (tail remaining)
                                       (offset + (operandSize ret))

calculateDbOffsets :: Instruction -> Int -> Instruction
calculateDbOffsets inst offset = do
    inst {
        instructionOffset = offset,
        operands = calculateDbOperandOffsets (operands inst)
                    (offset + (commandSize (command inst)))
    }

calculateDefaultOffsets :: Instruction -> Int -> Instruction
calculateDefaultOffsets inst offset = inst { instructionOffset = offset }

calculateOperandsSize :: Instruction -> Int
calculateOperandsSize inst = sum (map operandSize (operands inst))

calculateCommandSize :: Instruction -> Int
calculateCommandSize inst = commandSize (command inst) +
                            calculateOperandsSize inst

calculateInstructionOffsets :: [Instruction] -> Int -> [Instruction]
calculateInstructionOffsets [] _ = []
calculateInstructionOffsets instructions offset = do
    let current = head instructions

    let calculator = case command current of
         "mov" -> calculateMovOffsets
         "jmp" -> calculateJmpOffsets
         "db"  -> calculateDbOffsets
         _     -> calculateDefaultOffsets

    let ret = calculator current offset
    let size = calculateCommandSize ret

    [ret] ++ calculateInstructionOffsets (tail instructions) (offset + size)

calculateSectionOffsets :: [Section] -> [Section]
calculateSectionOffsets [] = []
calculateSectionOffsets sections = do
    let current = head sections
    let ret = current {
        instructions = calculateInstructionOffsets (instructions current) 0
    }
    [ret] ++ calculateSectionOffsets (tail sections)

isQuote :: [Char] -> Bool
isQuote []  = False
isQuote [_] = False
isQuote str = (head str) == '"' && (last str) == '"'

renderDbOperand :: Operand -> [Int]
renderDbOperand operand = do
    case isQuote (text operand) of
        False -> [read (text operand) :: Int]
        True  -> map ord (init (drop 1 (text operand)))

renderDb :: Instruction -> [Int]
renderDb inst = concat (map renderDbOperand (operands inst))

renderEqu :: Instruction -> [Int]
renderEqu inst = [] -- equ should not render any bytes

-- Render the first operand of a jump instruction
renderJumpOperand :: Instruction -> [Int]
renderJumpOperand inst = 
    case stringToInt (text (head (operands inst))) of
        Nothing -> error("Failed to parse jump operand")
        Just i  -> renderInt (fromIntegral i :: Int32)

renderJmp :: Instruction -> [Int]
renderJmp inst = [0xe9] ++ renderJumpOperand inst

renderJe :: Instruction -> [Int]
renderJe inst = [0x0f, 0x84] ++ renderJumpOperand inst -- TODO: allow je short

renderJne :: Instruction -> [Int]
renderJne inst = [0x0f, 0x85] ++ renderJumpOperand inst

renderJl :: Instruction -> [Int]
renderJl inst = [0x0f, 0x8c] ++ renderJumpOperand inst

renderJle :: Instruction -> [Int]
renderJle inst = [0x0f, 0x8e] ++ renderJumpOperand inst

renderJg :: Instruction -> [Int]
renderJg inst = [0x0f, 0x8f] ++ renderJumpOperand inst

renderJge :: Instruction -> [Int]
renderJge inst = [0x0f, 0x8d] ++ renderJumpOperand inst

renderInstruction :: Instruction -> [Int]
renderInstruction inst = do
    case (command inst) of
         "syscall" -> renderSysCall inst
         "mov"     -> renderMov inst
         "db"      -> renderDb inst
         "equ"     -> renderEqu inst
         "jmp"     -> renderJmp inst
         "je"      -> renderJe inst
         "jne"     -> renderJne inst
         "jl"      -> renderJl inst
         "jle"     -> renderJle inst
         "jg"      -> renderJg inst
         "jge"     -> renderJge inst
         "cmp"     -> renderCmp inst
         "inc"     -> renderIncDec inst
         "dec"     -> renderIncDec inst

renderSysCall :: Instruction -> [Int]
renderSysCall _ = [0x0f, 0x05]

stringToInt :: [Char] -> Maybe Int
stringToInt input = do
    case reads input :: [(Int, [Char])] of
        [] -> Nothing
        x  -> if snd (head x) == [] then Just (fst (head x)) else Nothing

renderMov :: Instruction -> [Int]
renderMov inst = do
    let prefix = case text (head (operands inst)) of
         "rax" -> [0x48, 0xb8]
         "rbx" -> [0x48, 0xbb]
         "rcx" -> [0x48, 0xb9]
         "rdx" -> [0x48, 0xba]
         "rdi" -> [0x48, 0xbf]
         "rsi" -> [0x48, 0xbe]
         "rbp" -> [0x48, 0xbd]
         "rsp" -> [0x48, 0xbc]
         "r8"  -> [0x49, 0xb8]
         "r9"  -> [0x49, 0xb9]
         "r10" -> [0x49, 0xba]
         "r11" -> [0x49, 0xbb]
         "r12" -> [0x49, 0xbc]
         "r13" -> [0x49, 0xbd]
         "r14" -> [0x49, 0xbe]
         "r15" -> [0x49, 0xbf]

    let source = (operands inst) !! 1
    -- TODO: unhack. Check if there's a relocation instead.
    let parsed = case stringToInt (text source) of Nothing -> 0
                                                   Just i  -> i
    prefix ++ renderInt parsed

-- Convert an array of bits into a byte
bitsToByte :: [Int] -> Int
bitsToByte = foldl (\byte bit -> byte * 2 + bit) 0

registersStandard64 = ["rax", "rbx", "rcx", "rdx", "rdi", "rsi", "rbp", "rsp"]
registersExtended64 = ["r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15"]
registers32         = ["eax", "ebx", "ecx", "edx", "edi", "esi", "ebp", "esp"]
registers16         = ["ax",  "bx",  "cx",  "dx",  "di",  "si",  "bp",  "sp" ]
registersLow8       = ["al",  "bl",  "cl",  "dl"]
registersHigh8      = ["ah",  "bh",  "ch",  "dh"]

registers64 = registersStandard64 ++ registersExtended64
registers8 = registersLow8 ++ registersHigh8

registersAll = registers64 ++ registers32 ++ registers16 ++ registers8

-- Get the number of bits in the given register
bitsInRegister :: [Char] -> Int
bitsInRegister register
    | elem register registers64 = 64
    | elem register registers32 = 32
    | elem register registers16 = 16
    | elem register registers8  = 8

-- Check if a string matches a known register
isRegister :: [Char] -> Bool
isRegister register = elem register registersAll

-- Returns a flag indicating if the register is one of the new 64-bit ones
isExtendedRegister :: [Char] -> Int
isExtendedRegister register =
    if elem register registersExtended64 then 1 else 0

-- Gets the 3-bit code for an operand (as in cmp instructions)
registerOperand :: [Char] -> [Int]
registerOperand "rax" = [0, 0, 0]
registerOperand "rbx" = [0, 1, 1]
registerOperand "rcx" = [0, 0, 1]
registerOperand "rdx" = [0, 1, 0]
registerOperand "rdi" = [1, 1, 1]
registerOperand "rsi" = [1, 1, 0]
registerOperand "rbp" = [1, 0, 1]
registerOperand "rsp" = [1, 0, 0]
registerOperand "r8"  = [0, 0, 0]
registerOperand "r9"  = [0, 0, 1]
registerOperand "r10" = [0, 1, 0]
registerOperand "r11" = [0, 1, 1]
registerOperand "r12" = [1, 0, 0]
registerOperand "r13" = [1, 0, 1]
registerOperand "r14" = [1, 1, 0]
registerOperand "r15" = [1, 1, 1]
registerOperand "eax" = [0, 0, 0]
registerOperand "ebx" = [0, 1, 1]
registerOperand "ecx" = [0, 0, 1]
registerOperand "edx" = [0, 1, 0]
registerOperand "edi" = [1, 1, 1]
registerOperand "esi" = [1, 1, 0]
registerOperand "ebp" = [1, 0, 1]
registerOperand "esp" = [1, 0, 0]
registerOperand "ax"  = [0, 0, 0]
registerOperand "bx"  = [0, 1, 1]
registerOperand "cx"  = [0, 0, 1]
registerOperand "dx"  = [0, 1, 0]
registerOperand "di"  = [1, 1, 1]
registerOperand "si"  = [1, 1, 0]
registerOperand "bp"  = [1, 0, 1]
registerOperand "sp"  = [1, 0, 0]
registerOperand "al"  = [0, 0, 0]
registerOperand "bl"  = [0, 1, 1]
registerOperand "cl"  = [0, 0, 1]
registerOperand "dl"  = [0, 1, 0]
registerOperand "ah"  = [1, 0, 0]
registerOperand "bh"  = [1, 1, 1]
registerOperand "ch"  = [1, 0, 1]
registerOperand "dh"  = [1, 1, 0]

-- Render an inc or dec instruction
renderIncDec :: Instruction -> [Int]
renderIncDec inst = do
    let oper = text (head (operands inst))

    let bits = bitsInRegister oper
    let prefix = case bits of 8  -> [0xfe]
                              16 -> [0x66, 0xff]
                              32 -> [0xff]
                              64 -> [0x48, 0xff]

    let instructionBit = if command inst == "inc" then 0 else 1

    let byte = [1, 1, 0, 0,
                if command inst == "inc" then 0 else 1] ++
               registerOperand oper

    prefix ++ [bitsToByte byte]

--rax    48 ff
--eax    ff
--ax     66 ff
--al     fe

-- Special case where if the first operand is rax and the second operand is an
-- immediate, the instruction renders differently
isWeirdCmpCase :: Instruction -> Bool
isWeirdCmpCase inst = do
    let second = text (last (operands inst))
    let first = text (head (operands inst))

    first == "rax" && not (isRegister second)

-- Render a cmp instruction
renderCmp :: Instruction -> [Int]
renderCmp inst = do
    let first = text (head (operands inst))
    let second = text (last (operands inst))

    let isSecondImmediate = not (isRegister second)

    let firstByte = [0, 1, 0, 0, 1,
                     isExtendedRegister second,
                     0,
                     isExtendedRegister first]

    let secondByte = if not isSecondImmediate then [0, 0, 1, 1, 1, 0, 0, 1]
                     else if first == "rax" then [0, 0, 1, 1, 1, 1, 0, 1]
                     else [1, 0, 0, 0, 0, 0, 0, 1]

    let thirdByte = [1, 1] ++
                    (if isSecondImmediate then [1, 1, 1]
                     else registerOperand second) ++
                     registerOperand first

    -- In the weird cmp case, the whole third byte is left out
    let prefixBytes = [firstByte, secondByte] ++
                      if isWeirdCmpCase inst then [] else [thirdByte]

    let prefix = map bitsToByte prefixBytes

    let parsed = case stringToInt second of
                  Nothing -> error("Can't parse second operand: " ++ second)
                  Just i  -> fromIntegral i :: Int32

    -- Add immediate operand if needed
    prefix ++ if isSecondImmediate then renderInt parsed else []

toByteString :: [Int] -> BL.ByteString
toByteString input = BL.pack (map fromIntegral input)

renderSection :: Section -> [Int]
renderSection section = concat (map renderInstruction (instructions section))

renderStrTabEntry :: [Char] -> [Int]
renderStrTabEntry string = map ord string ++ [0]

renderStrTab :: [[Char]] -> [Int]
renderStrTab strings = concat (map renderStrTabEntry strings)

renderInt :: (Binary a) => a -> [Int]
renderInt src = reverse [fromIntegral o :: Int | o <- BL.unpack (encode src)]

data SectionHeaderType = SHT_NULL |
                         SHT_PROGBITS |
                         SHT_SYMTAB |
                         SHT_STRTAB |
                         SHT_RELA

sectionHeaderTypeToInt32 :: SectionHeaderType -> Int32
sectionHeaderTypeToInt32 SHT_NULL     = 0
sectionHeaderTypeToInt32 SHT_PROGBITS = 1
sectionHeaderTypeToInt32 SHT_SYMTAB   = 2
sectionHeaderTypeToInt32 SHT_STRTAB   = 3
sectionHeaderTypeToInt32 SHT_RELA     = 4

data SectionHeaderFlags = SHF_NONE | SHF_ALLOC_WRITE | SHF_ALLOC_EXEC -- TODO

sectionHeaderFlagsToInt64 :: SectionHeaderFlags -> Int64
sectionHeaderFlagsToInt64 SHF_NONE        = 0
sectionHeaderFlagsToInt64 SHF_ALLOC_EXEC  = 6
sectionHeaderFlagsToInt64 SHF_ALLOC_WRITE = 3

data SectionHeader = SectionHeader {
    sectionName     :: [Char],
    sectionData     :: [Int],
    sectionPadding  :: Int,
    sectionLinkName :: [Char],
    sh_name         :: Int32,
    sh_type         :: SectionHeaderType,
    sh_flags        :: SectionHeaderFlags,
    sh_addr         :: Int64,
    sh_offset       :: Int64,
    sh_size         :: Int64,
    sh_link         :: Int32,
    sh_info         :: Int32,
    sh_addralign    :: Int64,
    sh_entsize      :: Int64
}

linkSectionHeader :: [SectionHeader] -> SectionHeader -> SectionHeader
linkSectionHeader all header =
    header {
        sh_link = fromIntegral (getSectionHeaderIndex all
                                (sectionLinkName header)) :: Int32
    }

linkSectionHeaders :: [SectionHeader] -> [SectionHeader]
linkSectionHeaders sections = map (linkSectionHeader sections) sections

renderSectionHeader :: SectionHeader -> [Int]
renderSectionHeader sh =
    renderInt (sh_name sh) ++
    renderInt (sectionHeaderTypeToInt32 (sh_type sh)) ++
    renderInt (sectionHeaderFlagsToInt64 (sh_flags sh)) ++
    renderInt (sh_addr sh) ++
    renderInt (sh_offset sh) ++
    renderInt (sh_size sh) ++
    renderInt (sh_link sh) ++
    renderInt (sh_info sh) ++
    renderInt (sh_addralign sh) ++
    renderInt (sh_entsize sh)

getStrTabIndex :: [[Char]] -> [Char] -> Int
getStrTabIndex strtab find = length (fst (break (== find) strtab))

getStrTabOffsetInner :: [[Char]] -> [Char] -> Int32 -> Int32
getStrTabOffsetInner [] find _ = error ("strtab entry missing: " ++ show find)
getStrTabOffsetInner strtab find offset = do
    let current = head strtab

    let inner = getStrTabOffsetInner (tail strtab) find
                (offset + (fromIntegral (length current) :: Int32) + 1)

    if current == find then offset else inner

getStrTabOffset :: [[Char]] -> [Char] -> Int32
getStrTabOffset strtab find = getStrTabOffsetInner strtab find 0

calculateSectionHeaderOffsets :: Int -> [SectionHeader] -> [SectionHeader]
calculateSectionHeaderOffsets _ [] = []
calculateSectionHeaderOffsets offset headers = do
    let current = head headers
    let endOfData = offset + (fromIntegral (sh_size current) :: Int)
    let nextOffset = align endOfData 16

    let ret = current {
        sectionPadding = nextOffset - endOfData,
        sh_offset = fromIntegral offset :: Int64
    }

    [ret] ++ calculateSectionHeaderOffsets nextOffset (tail headers)

getSectionHeaderNameOffsets :: [[Char]] -> [SectionHeader] -> [SectionHeader]
getSectionHeaderNameOffsets shstrtab headers =
    [sh { sh_name = getStrTabOffset shstrtab (sectionName sh) } |
     sh <- headers]

-- Align value to the nearest alignment by rounding up
align :: Int -> Int -> Int
align value alignment
    | r == 0    = value
    | otherwise = value + (alignment - r)
    where r = mod value alignment

-- Find index of a section header by name
getSectionHeaderIndex :: [SectionHeader] -> [Char] -> Int
getSectionHeaderIndex headers name = do
    let broken = break (\s -> (sectionName s) == name) headers
    case snd broken of [] -> 0
                       _  -> length (fst broken)

-- Helper function for creating section headers of various types
createSectionHeader :: Section -> SectionHeader
createSectionHeader section = do
    let content = renderSection section

    SectionHeader {
        sectionName = "." ++ kind section,
        sectionData = content,
        sectionPadding = 0,
        sectionLinkName = "",
        sh_name = 0,
        sh_type = SHT_PROGBITS,
        sh_addr = 0,
        sh_offset = 0,
        sh_size = fromIntegral (length content) :: Int64,
        sh_link = 0,
        sh_info = 0,
        sh_entsize = 0,
        sh_flags = case kind section of "text" -> SHF_ALLOC_EXEC
                                        "data" -> SHF_ALLOC_WRITE,
        sh_addralign = case kind section of "text" -> 0x10
                                            "data" -> 0x4
    }

renderSectionData :: SectionHeader -> [Int]
renderSectionData header =
    if length (sectionData header) == 0
        then []
        else sectionData header ++ replicate (sectionPadding header) 0x00

renderSectionsData :: [SectionHeader] -> [Int]
renderSectionsData [] = []
renderSectionsData headers = concat (map renderSectionData headers)

data SymTabEntryType = STT_NOTYPE | STT_SECTION | STT_FILE

renderSymTabEntryType :: SymTabEntryType -> Int
renderSymTabEntryType STT_NOTYPE  = 0
renderSymTabEntryType STT_SECTION = 3
renderSymTabEntryType STT_FILE    = 4

data SymTabEntryVisibility = STV_DEFAULT

renderSymTabEntryVisibility :: SymTabEntryVisibility -> [Int]
renderSymTabEntryVisibility STV_DEFAULT = [0]

data SymTabEntryBinding = STB_LOCAL | STB_GLOBAL

renderSymTabEntryBinding :: SymTabEntryBinding -> Int
renderSymTabEntryBinding STB_LOCAL  = 0
renderSymTabEntryBinding STB_GLOBAL = 1

data SymTabEntry = SymTabEntry {
    symTabType    :: SymTabEntryType,
    symTabBinding :: SymTabEntryBinding,
    st_name       :: [Char],
    st_other      :: SymTabEntryVisibility,
    st_shndx      :: Int16, -- TODO: Find uint types
    st_value      :: Int64,
    st_size       :: Int64
}

renderSymTabInfo :: SymTabEntryBinding -> SymTabEntryType -> [Int]
renderSymTabInfo b t = [(shiftL (renderSymTabEntryBinding b) 4) +
                        (renderSymTabEntryType t)]

renderSymTabEntry :: [[Char]] -> SymTabEntry -> [Int]
renderSymTabEntry strtab entry =
    renderInt (fromIntegral (getStrTabOffset strtab (st_name entry))
        :: Int32) ++
    renderSymTabInfo (symTabBinding entry) (symTabType entry) ++
    renderSymTabEntryVisibility (st_other entry) ++
    renderInt (st_shndx entry) ++
    renderInt (st_value entry) ++
    renderInt (st_size entry)

sectionsToSymTabInner :: [SectionHeader] -> Int16 -> [SymTabEntry]
sectionsToSymTabInner [] _ = []
sectionsToSymTabInner sections index =
    [SymTabEntry {
        st_name  = "",
        symTabType = STT_SECTION,
        symTabBinding = STB_LOCAL,
        st_other = STV_DEFAULT,
        st_shndx = index,
        st_value = 0,
        st_size  = 0
    }] ++ sectionsToSymTabInner (tail sections) (succ index)

sectionsToSymTab :: [SectionHeader] -> [SymTabEntry]
sectionsToSymTab s = sectionsToSymTabInner s 1

labelsToSymTabInner :: [Section] -> [[Char]] -> Int16 -> [SymTabEntry]
labelsToSymTabInner [] _ _ = []
labelsToSymTabInner sections globals index = do
    let section = head sections

    let labelOffsets = concat [[(l, instructionOffset i) | l <- labels i] |
                               i <- instructions section]

    let ret = [SymTabEntry {
        st_name  = fst label,
        symTabType = STT_NOTYPE,
        st_other = STV_DEFAULT,
        symTabBinding = if elem (fst label) globals then STB_GLOBAL
                        else STB_LOCAL,
        st_shndx = index,
        st_value = fromIntegral (snd label) :: Int64,
        st_size  = 0
    } | label <- labelOffsets]

    ret ++ labelsToSymTabInner (tail sections) globals (succ index)

labelsToSymTab :: [Section] -> [[Char]] -> [SymTabEntry]
labelsToSymTab sections globals = labelsToSymTabInner sections globals 1

data Relocation = Relocation {
    sourceSection     :: Section,
    sourceOperand     :: Operand,
    targetSection     :: Section,
    targetInstruction :: Instruction
}

findLabel :: [Section] -> [Char] -> Maybe (Section, Instruction)
findLabel [] labelName = Nothing
findLabel sections labelName = do
    let section = head sections

    let broken = break (\i -> elem labelName (labels i)) (instructions section)

    if length(snd broken) == 0
        then findLabel (tail sections) labelName
        else Just (section, head (snd broken))

getRelocation :: [Section] -> Section -> Operand -> [Relocation]
getRelocation sections section operand = do
    case findLabel sections (text operand) of
        Nothing     -> []
        Just (s, i) -> [Relocation {
                           sourceSection     = section,
                           sourceOperand     = operand,
                           targetSection     = s,
                           targetInstruction = i
                       }]

-- Check if the given command is a type of jump instruction
isJump :: [Char] -> Bool
isJump command = elem command ["jmp", "je", "jne", "jl", "jle", "jg", "jge"]

getRelocationsInner :: [Section] -> [Section] -> [Relocation]
getRelocationsInner [] _ = []
getRelocationsInner remainingSections allSections = do
    let section = head remainingSections
    let jumpFilter inst = not (isJump (command inst))
    let nonJumpInstructions = filter jumpFilter (instructions section)
    let allOperands = concat (map operands nonJumpInstructions)

    let ret = concat (map (getRelocation allSections section) allOperands)
    ret ++ getRelocationsInner (tail remainingSections) allSections

getRelocations :: [Section] -> [Relocation]
getRelocations sections = getRelocationsInner sections sections

renderRelocation :: Relocation -> [Int]
renderRelocation relo =
    renderInt (fromIntegral (operandOffset (sourceOperand relo)) :: Int64) ++
    renderInt (fromIntegral (sectionIndex (targetSection relo)) :: Int32) ++
    renderInt (fromIntegral (sectionIndex (sourceSection relo)) :: Int32) ++
    renderInt (fromIntegral (instructionOffset (targetInstruction relo)) ::
                 Int64)

getSymbolLength :: [Char] -> [Section] -> Int
getSymbolLength name sections = do
    case findLabel sections name of
         Nothing     -> error("Can't calculate symbol length of " ++ name)
         Just (s, i) -> calculateOperandsSize i

getEquValue :: Instruction -> [Section] -> [Char]
getEquValue inst sections = do
    let oper = text (head (operands inst))

    if (take 2 oper) /= "$-"
        then oper
        else show (getSymbolLength (drop 2 oper) sections)

replaceEqusOperands :: [Operand] -> [Section] -> [Operand]
replaceEqusOperands [] _ = []
replaceEqusOperands opers allSections = do
    let current = head opers
    let operText = text current

    let newText = case findLabel allSections operText of
         Nothing  -> operText
         Just res -> case command (snd res) of
             "equ" -> getEquValue (snd res) allSections
             _     -> operText

    [current {
        text = newText
    }] ++ replaceEqusOperands (tail opers) allSections

replaceEqusSection :: [Instruction] -> [Section] -> [Instruction]
replaceEqusSection [] allSections = []
replaceEqusSection instructions allSections = do
    let current = head instructions

    [current {
        operands = replaceEqusOperands (operands current) allSections
    }] ++ replaceEqusSection (tail instructions) allSections

replaceEqus :: [Section] -> [Section] -> [Section]
replaceEqus [] _ = []
replaceEqus remainingSections allSections = do
    let current = head remainingSections

    [current {
        instructions = replaceEqusSection (instructions current) allSections
    }] ++ replaceEqus (tail remainingSections) allSections

allLabels :: [Section] -> [[Char]]
allLabels sections = concat (map labels (concat (map instructions sections)))

labelOffset :: Section -> [Char] -> Int
labelOffset section labelName = do
    case findLabel [section] labelName of
         Nothing  -> error("Unable to find label [" ++ labelName ++ "]")
         Just res -> instructionOffset (snd res)

-- Replace label in jump with RIP-relative address
applyJump :: Section -> Instruction -> Instruction
applyJump section inst = if not (isJump (command inst)) then inst else do
    -- Offset of the next instruction after the current jump (RIP's value)
    let sourceOffset = (instructionOffset inst) + (calculateCommandSize inst)
    let targetOffset = labelOffset section (text (head (operands inst)))

    inst {
        operands = [(head (operands inst)) {
            text = show (targetOffset - sourceOffset)
        }]
    }

-- Replace jumps with RIP-relative addresses by section
applyJumps :: Section -> Section
applyJumps section = section {
    instructions = map (applyJump section) (instructions section) }

reloHeaders :: [Relocation] -> [SectionHeader]
reloHeaders [] = []
reloHeaders relocations = do
    let rendered = concat (map renderRelocation relocations)

    [SectionHeader {
        sectionName = ".rela.text",
        sectionData = rendered,
        sectionPadding = 0,
        sectionLinkName = ".symtab",
        sh_name = 0,
        sh_type = SHT_RELA,
        sh_flags = SHF_NONE,
        sh_addr = 0,
        sh_offset = 0,
        sh_size = fromIntegral (length rendered) :: Int64,
        sh_link = 0,
        sh_info = 0x2, -- TODO: link to text section
        sh_addralign = 0x4,
        sh_entsize = 0x18
    }]

main :: IO ()
main = do
    args <- getArgs
    let filename = case length args of 1 -> head args
                                       _ -> error("Usage: basm <filename>")

    contents <- readFile filename

    let (tempSections, globals) = parse contents
    let sectionsTemp = calculateSectionOffsets tempSections
    let sectionsTemp2 = replaceEqus sectionsTemp sectionsTemp
    let sections = map applyJumps sectionsTemp2

    let shRelo = reloHeaders (getRelocations sections)

    let e_ehsize    = 64 :: Int16
    let e_shentsize = 64 :: Int16
    let e_shoff     = 64 :: Int64

    let strtab = ["", filename] ++ allLabels sections

    let userSectionHeaders = map createSectionHeader sections
    let dynamicSh = userSectionHeaders ++ shRelo

    let shstrtab = ["", ".shstrtab", ".symtab", ".strtab"] ++
                   map sectionName dynamicSh

    let renderedStrTab = renderStrTab(strtab)
    let renderedShStrTab = renderStrTab(shstrtab)

    let stNull = SymTabEntry {
        st_name       = "",
        symTabType    = STT_NOTYPE,
        symTabBinding = STB_LOCAL,
        st_other      = STV_DEFAULT,
        st_shndx      = 0,
        st_value      = 0,
        st_size       = 0
    }

    let stFile = SymTabEntry {
        st_name       = filename,
        symTabType    = STT_FILE,
        symTabBinding = STB_LOCAL,
        st_other      = STV_DEFAULT,
        st_shndx      = 65521,
        st_value      = 0,
        st_size       = 0
    }

    let stSections = sectionsToSymTab userSectionHeaders
    let stLabels = reverse (labelsToSymTab sections globals)

    let symtab = [stNull, stFile] ++ stSections ++ stLabels

    let renderedSymTab = concat (map (renderSymTabEntry strtab) symtab)

    let headers = [SectionHeader {
        sectionName = "",
        sectionData = [],
        sectionPadding = 0,
        sectionLinkName = "",
        sh_name = 0,
        sh_type = SHT_NULL,
        sh_flags = SHF_NONE,
        sh_addr = 0,
        sh_offset = 0,
        sh_size = 0,
        sh_link = 0,
        sh_info = 0,
        sh_addralign = 0,
        sh_entsize = 0
    }] ++ userSectionHeaders ++ [SectionHeader {
        sectionName = ".shstrtab",
        sectionData = renderedShStrTab,
        sectionPadding = 0,
        sectionLinkName = "",
        sh_name = 0,
        sh_type = SHT_STRTAB,
        sh_flags = SHF_NONE,
        sh_addr = 0,
        sh_offset = 0,
        sh_size = fromIntegral (length renderedShStrTab) :: Int64,
        sh_link = 0,
        sh_info = 0,
        sh_addralign = 0x1,
        sh_entsize = 0
    }, SectionHeader {
        sectionName = ".symtab",
        sectionData = renderedSymTab,
        sectionPadding = 0,
        sectionLinkName = ".strtab",
        sh_name = 0,
        sh_type = SHT_SYMTAB,
        sh_flags = SHF_NONE,
        sh_addr = 0,
        sh_offset = 0,
        sh_size = fromIntegral (length renderedSymTab) :: Int64,
        sh_link = 0,
        sh_info = fromIntegral ((length symtab) - 1) :: Int32,
        sh_addralign = 0x8,
        sh_entsize = 0x18
    }, SectionHeader {
        sectionName = ".strtab",
        sectionData = renderedStrTab,
        sectionPadding = 0,
        sectionLinkName = "",
        sh_name = 0,
        sh_type = SHT_STRTAB,
        sh_flags = SHF_NONE,
        sh_addr = 0,
        sh_offset = 0,
        sh_size = fromIntegral (length renderedStrTab) :: Int64,
        sh_link = 0,
        sh_info = 0,
        sh_addralign = 0x1,
        sh_entsize = 0
    }] ++ shRelo

    let firstOffset = (fromIntegral e_ehsize :: Int) + (length headers) *
                      (fromIntegral e_shentsize :: Int)

    -- Only calculate offsets on the headers after the null header
    let headersTail = calculateSectionHeaderOffsets firstOffset (tail headers)
    let headers2 = getSectionHeaderNameOffsets shstrtab .
                   linkSectionHeaders $ ([head headers] ++ headersTail)

    let e_shnum = fromIntegral (length headers) :: Int16
    let e_shstrndx = fromIntegral (getSectionHeaderIndex headers2 ".shstrtab")
                                  :: Int16

    let header = [0x7f, 0x45, 0x4c, 0x46,                      -- magic
                  0x02,                                        -- 64-bit
                  0x01,                                        -- little endian
                  0x01,                                        -- ELF version
                  0x00,                                        -- System V ABI
                  0x00,                                        -- ABI version
                  0x00, 0x00, 0x00, 0x00,
                  0x00, 0x00, 0x00,                            -- padding
                  0x01, 0x00,                                  -- relocatable
                  0x3e, 0x00,                                  -- e_machine
                  0x01, 0x00, 0x00, 0x00,                      -- e_version
                  0x00, 0x00, 0x00, 0x00,                      -- entry point?
                  0x00, 0x00, 0x00, 0x00,
                  0x00, 0x00, 0x00, 0x00,                      -- e_phoff
                  0x00, 0x00, 0x00, 0x00] ++
                  renderInt e_shoff ++                         -- e_shoff
                 [0x00, 0x00, 0x00, 0x00] ++                   -- e_flags
                  renderInt e_ehsize ++                        -- e_ehsize
                 [0x00, 0x00,                                  -- e_phentsize?
                  0x00, 0x00] ++                               -- e_phnum
                  renderInt e_shentsize ++                     -- e_shentsize
                  renderInt e_shnum ++                         -- e_shnum
                  renderInt e_shstrndx :: [Int]                -- e_shstrndx

    let renderedSectionHeaders = map renderSectionHeader headers2

    let rendered = header ++
                   concat renderedSectionHeaders ++
                   renderSectionsData headers2

    BL.writeFile "basm.o" (toByteString rendered)
