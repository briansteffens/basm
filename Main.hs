module Main where

import Data.Int
import Data.List
import Data.Char
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Debug.Trace (trace)

data Section = Section {
    kind :: [Char],
    instructions :: [Instruction],
    sectionIndex :: Int
}

data Instruction = Instruction {
    labels :: [[Char]],
    command :: [Char],
    operands :: [Operand],
    instructionOffset :: Int
}

data Operand = Operand {
    text :: [Char],
    operandOffset :: Int,
    operandSize :: Int
}

data QuoteChar = QuoteChar {
    char :: Char,
    quoted :: Bool,
    bracketed :: Bool
}

parseQuotesInner :: [Char] -> Maybe Char -> Bool -> Bool -> [QuoteChar]
parseQuotesInner [] _ _ _ = []
parseQuotesInner input previous inQuote inBracket = do
    let current = head input
    let escaped = case previous of Just c  -> c == '\\'
                                   Nothing -> False
    let isQuote = (not escaped) && current == '"'
    let newInQuote = case inQuote of False -> isQuote
                                     True  -> not isQuote
    let tempNewInBracket = case inBracket of False -> current == '['
                                             True  -> current /= ']'
    let newInBracket = (not inQuote) && tempNewInBracket
    let inner = parseQuotesInner (tail input) (Just current) newInQuote
                                 newInBracket
    [(QuoteChar current (inQuote && (not isQuote))
      (newInBracket || inBracket))] ++ inner

parseQuotes :: [Char] -> [QuoteChar]
parseQuotes input = parseQuotesInner input Nothing False False

showQuoteChar :: QuoteChar -> [Char]
showQuoteChar parsed =
    [(char parsed)] ++
    (if (quoted parsed) then " quoted" else "") ++
    (if (bracketed parsed) then " bracketed" else "")

showQuoteChars :: [QuoteChar] -> [Char]
showQuoteChars parsed = intercalate "\n" (map showQuoteChar parsed)

stripComment :: [QuoteChar] -> [QuoteChar]
stripComment input = fst (break (\s -> (char s) == ';' &&
                                       (not (quoted s)) &&
                                       (not (bracketed s))) input)

isDelimiter :: Char -> Bool
isDelimiter input = input == ' ' ||
                    input == ':' ||
                    input == '\t' ||
                    input == ','

splitQuoteCharsInner :: [QuoteChar] -> [QuoteChar] -> [[QuoteChar]]
splitQuoteCharsInner [] accumulator = [accumulator]
splitQuoteCharsInner remaining accumulator = do
    let current = head remaining
    let theRest = tail remaining

    let split = (isDelimiter (char current)) &&
                (not (quoted current)) &&
                (not (bracketed current))

    let ret = case split of False -> []
                            True  -> accumulator

    -- Semicolon is an abnormal delimiter: add it to the return
    let ret2 = case (char current) of ':' -> accumulator ++ [current]
                                      _   -> ret

    let newNext = case split of False -> accumulator ++ [current]
                                True  -> []

    [ret2] ++ (splitQuoteCharsInner theRest newNext)

splitQuoteChars :: [QuoteChar] -> [[QuoteChar]]
splitQuoteChars input = splitQuoteCharsInner input []

removeEmpty :: [[QuoteChar]] -> [[QuoteChar]]
removeEmpty input = [s | s <- input, not (null s)]

toText :: [QuoteChar] -> [Char]
toText x = [char s | s <- x]

toTextArray :: [[QuoteChar]] -> [[Char]]
toTextArray x = [toText s | s <- x]

processLine :: [Char] -> [[Char]]
processLine = toTextArray .
              removeEmpty .
              splitQuoteChars .
              stripComment .
              parseQuotes

removeEmptyLines :: [[[Char]]] -> [[[Char]]]
removeEmptyLines input = [i | i <- input, (length i) > 0]

showLine lines = "LINE:\n  " ++ (intercalate "\n  " lines)

mergeLabelsInner :: [[Char]] -> [[[Char]]] -> [[[Char]]]
mergeLabelsInner _ [] = []
mergeLabelsInner labels remaining = do
    let current = head remaining

    let allLabels = all (\s -> last s == ':') current

    -- TODO: more expressive way to do these next two lines?
    let ret = case allLabels of False -> labels ++ current
                                True  -> []

    let innerLabels = case allLabels of False -> []
                                        True  -> labels ++ current

    let inner = mergeLabelsInner innerLabels (tail remaining)

    [ret] ++ inner

mergeLabels :: [[[Char]]] -> [[[Char]]]
mergeLabels input = mergeLabelsInner [] input

commandSize :: [Char] -> Int
commandSize cmd =
    case cmd of "syscall" -> 2
                "mov"     -> 2
                "db"      -> 0
                "%define" -> 1   -- TODO: hack

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

calculateSysCallOffsets :: Instruction -> Int -> Instruction
calculateSysCallOffsets inst offset = do
    inst {
        instructionOffset = offset
    }

calculateDbOffsets :: Instruction -> Int -> Instruction
calculateDbOffsets inst offset = do
    inst {
        instructionOffset = offset
    }

calculateInstructionOffsets :: [Instruction] -> Int -> [Instruction]
calculateInstructionOffsets [] _ = []
calculateInstructionOffsets instructions offset = do
    let current = head instructions

    let calculator = case command current of "mov"     -> calculateMovOffsets
                                             "syscall" -> calculateSysCallOffsets
                                             "db"      -> calculateDbOffsets

    let ret = calculator current offset

    let size = (commandSize (command current)) +
               sum [operandSize o | o <- operands ret]

    [ret] ++ calculateInstructionOffsets (tail instructions) (offset + size)

calculateSectionOffsets :: [Section] -> [Section]
calculateSectionOffsets [] = []
calculateSectionOffsets sections = do
    let current = head sections
    let ret = current {
        instructions = calculateInstructionOffsets (instructions current) 0
    }
    [ret] ++ calculateSectionOffsets (tail sections)

parseOperands :: [[Char]] -> Int -> [Operand]
parseOperands [] _ = []
parseOperands operands offset = do
    let ret = Operand (head operands) offset 0

    let inner = parseOperands (tail operands) (offset + (operandSize ret))

    [ret] ++ inner

parseInstructionsInner :: [[[Char]]] -> Int -> [Instruction]
parseInstructionsInner [] _ = []
parseInstructionsInner lines offset = do
    let current = head lines
    let parts = break (\s -> last s /= ':') current
    let labels = [init l | l <- fst parts]
    let nonLabel = snd parts
    let operandStrings = tail nonLabel
    let command = head nonLabel

    let size = commandSize command

    let operands = parseOperands operandStrings (offset + size)
    let instruction = Instruction labels command operands offset

    let operandsSize = sum [operandSize o | o <- operands]

    [instruction] ++ parseInstructionsInner (tail lines)
                                            (offset + size + operandsSize)

parseInstructions :: [[[Char]]] -> [Instruction]
parseInstructions lines = parseInstructionsInner lines 0

parseSectionsInner :: [[[Char]]] -> [Char] -> Int -> [Section]
parseSectionsInner [] _ _ = []
parseSectionsInner lines kind index = do
    let broken = break (\s -> head s == "section") lines

    let instructions = parseInstructions (fst broken)

    let ret = Section kind instructions index

    let nextSection = drop 1 ((head (snd broken)) !! 1)

    let anyLeft = not (null (snd broken))
    let remaining = case anyLeft of False -> []
                                    True  -> tail (snd broken)

    [ret] ++ parseSectionsInner remaining nextSection (succ index)

parseSections :: [[[Char]]] -> [Section]
parseSections lines = parseSectionsInner lines "base" 0

isQuote :: [Char] -> Bool
isQuote str =
    case (length str) of
        0 -> False
        1 -> False
        _ -> (head str) == '"' && (last str) == '"'

renderDbOperand :: Operand -> [Int]
renderDbOperand operand = do
    case isQuote (text operand) of
        False -> [read (text operand) :: Int]
        True  -> map ord (init (drop 1 (text operand)))

renderDb :: Instruction -> [Int]
renderDb inst = concat (map renderDbOperand (operands inst))

renderInstruction :: Instruction -> [Int]
renderInstruction inst = do
    case (command inst) of "syscall" -> (renderSysCall inst)
                           "mov"     -> (renderMov inst)
                           "db"      -> (renderDb inst)

renderSysCall :: Instruction -> [Int]
renderSysCall inst = [0x0f, 0x05]

stringToInt :: [Char] -> Maybe Int
stringToInt input = do
    let parsed = reads input :: [(Int, [Char])]
    case parsed of
        [] -> Nothing
        x  -> case (length (snd (head x))) of
            0 -> Just (fst (head x))
            _ -> Nothing

renderMov :: Instruction -> [Int]
renderMov inst = do
    let prefix = case (text (head (operands inst))) of "rax" -> [0x48, 0xb8]
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
    let parsed = case (stringToInt (text source)) of Nothing -> 0
                                                     Just i  -> i
    prefix ++ intToBytes parsed

toByteString :: [Int] -> BL.ByteString
toByteString input = BL.pack (map fromIntegral input)

renderSection :: Section -> [Int]
renderSection section = do
    concat (map renderInstruction (instructions section))

renderStrTab :: [[Char]] -> [Int]
renderStrTab strings = concat [(map ord s) ++ [0] | s <- strings]

-- TODO: generic
intToBytes :: Int -> [Int]
intToBytes src = reverse [fromIntegral o :: Int | o <- BL.unpack (encode src)]

renderInt16 :: Int16 -> [Int]
renderInt16 src = reverse [fromIntegral o :: Int | o <- BL.unpack (encode src)]

renderInt32 :: Int32 -> [Int]
renderInt32 src = reverse [fromIntegral o :: Int | o <- BL.unpack (encode src)]

renderInt64 :: Int64 -> [Int]
renderInt64 src = reverse [fromIntegral o :: Int | o <- BL.unpack (encode src)]

data SectionHeaderType = SHT_NULL |
                         SHT_PROGBITS |
                         SHT_SYMTAB |
                         SHT_STRTAB |
                         SHT_RELA

sectionHeaderTypeToInt32 :: SectionHeaderType -> Int32
sectionHeaderTypeToInt32 sht = case sht of SHT_NULL     -> 0
                                           SHT_PROGBITS -> 1
                                           SHT_SYMTAB   -> 2
                                           SHT_STRTAB   -> 3
                                           SHT_RELA     -> 4

data SectionHeaderFlags = SHF_NONE | SHF_ALLOC_WRITE | SHF_ALLOC_EXEC -- TODO

sectionHeaderFlagsToInt64 :: SectionHeaderFlags -> Int64
sectionHeaderFlagsToInt64 shf = case shf of SHF_NONE        -> 0  -- TODO
                                            SHF_ALLOC_EXEC  -> 6
                                            SHF_ALLOC_WRITE -> 3

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

renderSectionHeader :: SectionHeader -> [Int]
renderSectionHeader sh =
    renderInt32 (sh_name sh) ++
    renderInt32 (sectionHeaderTypeToInt32 (sh_type sh)) ++
    renderInt64 (sectionHeaderFlagsToInt64 (sh_flags sh)) ++
    renderInt64 (sh_addr sh) ++
    renderInt64 (sh_offset sh) ++
    renderInt64 (sh_size sh) ++
    renderInt32 (sh_link sh) ++
    renderInt32 (sh_info sh) ++
    renderInt64 (sh_addralign sh) ++
    renderInt64 (sh_entsize sh)

getStrTabIndex :: [[Char]] -> [Char] -> Int
getStrTabIndex strtab find = length (fst (break (== find) strtab))

getStrTabOffsetInner :: [[Char]] -> [Char] -> Int32 -> Int32
getStrTabOffsetInner [] find _ = error ("strtab entry not found: " ++
                                        (show find))
getStrTabOffsetInner strtab find offset = do
    let current = head strtab

    let inner = getStrTabOffsetInner (tail strtab) find
                (offset + (fromIntegral (length current) :: Int32) + 1)

    case current == find of True  -> offset
                            False -> inner

getStrTabOffset :: [[Char]] -> [Char] -> Int32
getStrTabOffset strtab find = getStrTabOffsetInner strtab find 0

calculateSectionHeaderOffsets :: [SectionHeader] -> Int -> [SectionHeader]
calculateSectionHeaderOffsets [] _ = []
calculateSectionHeaderOffsets headers offset = do
    let current = head headers
    let endOfData = offset + (fromIntegral (sh_size current) :: Int)
    let nextOffset = align endOfData 16

    let ret = current {
        sectionPadding = nextOffset - endOfData,
        sh_offset = fromIntegral offset :: Int64
    }

    [ret] ++ calculateSectionHeaderOffsets (tail headers) nextOffset

getSectionHeaderNameOffsets :: [SectionHeader] -> [[Char]] -> [SectionHeader]
getSectionHeaderNameOffsets headers shstrtab =
    [sh { sh_name = getStrTabOffset shstrtab (sectionName sh) } |
     sh <- headers]

-- Align value to the nearest alignment by rounding up
align :: Int -> Int -> Int
align value alignment =
    case (mod value alignment) of 0 -> value
                                  r -> value + (alignment - r)

-- Find index of a section header by name
getSectionHeaderIndex :: [SectionHeader] -> [Char] -> Int
getSectionHeaderIndex headers name = do
    let broken = break (\s -> (sectionName s) == name) headers
    case length (snd (broken)) of 0 -> 0
                                  _ -> length (fst broken)

-- Helper function for creating section headers of various types
createSectionHeaderInner :: Section -> SectionHeaderFlags -> Int64 ->
                            SectionHeader
createSectionHeaderInner section flags addralign = do
    let content = renderSection section
    SectionHeader {
        sectionName = "." ++ kind section,
        sectionData = content,
        sectionPadding = 0,
        sectionLinkName = "",
        sh_name = 0,
        sh_type = SHT_PROGBITS,
        sh_flags = flags,
        sh_addr = 0,
        sh_offset = 0,
        sh_size = fromIntegral (length content) :: Int64,
        sh_link = 0,
        sh_info = 0,
        sh_addralign = addralign,
        sh_entsize = 0
    }

createSectionHeader :: Section -> [SectionHeader]
createSectionHeader section =
    case (kind section) of
        "text" -> [createSectionHeaderInner section SHF_ALLOC_EXEC 0x10]
        "data" -> [createSectionHeaderInner section SHF_ALLOC_WRITE 0x4]
        "base" -> []

renderSectionData :: SectionHeader -> [Int]
renderSectionData header =
    case (length (sectionData header)) of
        0 -> []
        _ -> (sectionData header) ++ replicate (sectionPadding header) 0x00

renderSectionsData :: [SectionHeader] -> [Int]
renderSectionsData [] = []
renderSectionsData headers = concat (map renderSectionData headers)

data SymTabEntryType = STT_NOTYPE | STT_SECTION | STT_FILE

renderSymTabEntryType :: SymTabEntryType -> [Int]
renderSymTabEntryType t =
    [case t of
        STT_NOTYPE  -> 0
        STT_SECTION -> 3
        STT_FILE    -> 4]

data SymTabEntryVisibility = STV_DEFAULT

renderSymTabEntryVisibility :: SymTabEntryVisibility -> [Int]
renderSymTabEntryVisibility v =
    [case v of
        STV_DEFAULT -> 0]

data SymTabEntry = SymTabEntry {
    st_name  :: [Char],
    st_info  :: SymTabEntryType,
    st_other :: SymTabEntryVisibility,
    st_shndx :: Int16, -- TODO: Find uint types
    st_value :: Int64,
    st_size  :: Int64
}

renderSymTabEntry :: [[Char]] -> SymTabEntry -> [Int]
renderSymTabEntry strtab entry =
    renderInt32 (fromIntegral (getStrTabOffset strtab (st_name entry))
        :: Int32) ++
    renderSymTabEntryType (st_info entry) ++
    renderSymTabEntryVisibility (st_other entry) ++
    renderInt16 (st_shndx entry) ++
    renderInt64 (st_value entry) ++
    renderInt64 (st_size entry)

sectionsToSymTabInner :: [SectionHeader] -> Int16 -> [SymTabEntry]
sectionsToSymTabInner [] _ = []
sectionsToSymTabInner sections index =
    [SymTabEntry {
        st_name  = "",
        st_info  = STT_SECTION,
        st_other = STV_DEFAULT,
        st_shndx = index,
        st_value = 0,
        st_size  = 0
    }] ++ sectionsToSymTabInner (tail sections) (succ index)

sectionsToSymTab :: [SectionHeader] -> [SymTabEntry]
sectionsToSymTab s = sectionsToSymTabInner s 1

labelsToSymTabInner :: [Section] -> Int16 -> [SymTabEntry]
labelsToSymTabInner [] _ = []
labelsToSymTabInner sections index = do
    let section = head sections

    let labelOffsets = concat [[(l, instructionOffset i) | l <- labels i] |
                               i <- instructions section]

    let ret = [SymTabEntry {
        st_name  = fst label,
        st_info  = STT_NOTYPE,
        st_other = STV_DEFAULT,
        st_shndx = index,
        st_value = fromIntegral (snd label) :: Int64,
        st_size  = 0
    } | label <- labelOffsets]

    ret ++ labelsToSymTabInner (tail sections) (succ index)

labelsToSymTab :: [Section] -> [SymTabEntry]
labelsToSymTab sections = labelsToSymTabInner sections 1

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
    let found = length (snd broken) > 0

    case length (snd broken) > 0 of
        True  -> Just (section, head (snd broken))
        False -> findLabel (tail sections) labelName

getRelocation :: [Section] -> Section -> Operand -> [Relocation]
getRelocation sections section operand = do
    case findLabel sections (text operand) of
        Nothing -> []
        Just (s, i)  -> [Relocation {
                            sourceSection     = section,
                            sourceOperand     = operand,
                            targetSection     = s,
                            targetInstruction = i
                        }]

getRelocationsInner :: [Section] -> [Section] -> [Relocation]
getRelocationsInner [] _ = []
getRelocationsInner remainingSections allSections = do
    let section = head remainingSections
    let allOperands = concat [operands i | i <- instructions section]

    let ret = concat (map (getRelocation allSections section) allOperands)
    ret ++ getRelocationsInner (tail remainingSections) allSections

getRelocations :: [Section] -> [Relocation]
getRelocations sections = getRelocationsInner sections sections

renderRelocation :: Relocation -> [Int]
renderRelocation relo =
    renderInt64 (fromIntegral (operandOffset (sourceOperand relo)) :: Int64) ++
    renderInt32 (fromIntegral (sectionIndex (targetSection relo)) :: Int32) ++
    renderInt32 (fromIntegral (sectionIndex (sourceSection relo)) :: Int32) ++
    renderInt64 (fromIntegral (instructionOffset (targetInstruction relo)) ::
                 Int64)

allLabels :: [Section] -> [[Char]]
allLabels sections = do
    let allInstructions = concat [instructions s | s <- sections]
    concat [labels i | i <- allInstructions]

main :: IO ()
main = do
    contents <- getContents

    let sourceLines = lines contents
    let processed = map processLine sourceLines
    let merged = mergeLabels processed
    let emptied = removeEmptyLines merged
    let tempSections = parseSections emptied
    let sections = calculateSectionOffsets tempSections
    let sectionsAfterBase = (tail sections)
    --putStr (showSections sectionsAfterBase)

    let relocations = getRelocations sectionsAfterBase
    let renderedReloTab = concat (map renderRelocation relocations)

    let shRelo = SectionHeader {
        sectionName = ".rela.text",
        sectionData = renderedReloTab,
        sectionPadding = 0,
        sectionLinkName = ".symtab",
        sh_name = 0,
        sh_type = SHT_RELA,
        sh_flags = SHF_NONE,
        sh_addr = 0,
        sh_offset = 0,
        sh_size = fromIntegral (length renderedReloTab) :: Int64,
        sh_link = 0,
        --sh_info = fromIntegral (length relocations) :: Int32,
        sh_info = 0x2, -- TODO ??
        sh_addralign = 0x4,
        sh_entsize = 0x18
    }

    let e_ehsize = 64 :: Int16
    let e_shentsize = 64 :: Int16
    let e_shoff = 64 :: Int64

    let filename = "test2.asm"

    let strtab = ["", filename] ++ allLabels sectionsAfterBase

    let shstrtab = ["",         -- TODO calculate
                    ".data",
                    ".text",
                    ".shstrtab",
                    ".symtab",
                    ".strtab",
                    ".rela.text"]

    let renderedStrTab = renderStrTab(strtab)
    let renderedShStrTab = renderStrTab(shstrtab)

    let userSectionHeaders = concat (map createSectionHeader sections)

    let stNull = SymTabEntry {
        st_name  = "",
        st_info  = STT_NOTYPE,
        st_other = STV_DEFAULT,
        st_shndx = 0,
        st_value = 0,
        st_size  = 0
    }

    let stFile = SymTabEntry {
        st_name  = filename,
        st_info  = STT_FILE,
        st_other = STV_DEFAULT,
        st_shndx = 65521,
        st_value = 0,
        st_size  = 0
    }

    let stSections = sectionsToSymTab userSectionHeaders
    let stLabels = labelsToSymTab sectionsAfterBase

    let symtab = [stNull, stFile] ++ stSections ++ stLabels

    let renderedSymTab = concat (map (renderSymTabEntry strtab) symtab)

    let headers = userSectionHeaders ++ [SectionHeader {
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
        sh_info = fromIntegral (length symtab) :: Int32,
        sh_addralign = 0x4,
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
    }, shRelo]

    let e_ehsize = 64 :: Int16
    let e_shentsize = 64 :: Int16
    let e_shoff = 64 :: Int64

    -- Calculate first section offset
    -- TODO: there must be a way to avoid 3 casts here?
    let shLen = (succ (length headers)) * (fromIntegral e_shentsize :: Int)
    let firstOffset = (fromIntegral e_ehsize :: Int) + shLen
    let headers2 = calculateSectionHeaderOffsets headers
                   (fromIntegral firstOffset :: Int)

    -- Prepend null section header
    let headers3 = [SectionHeader {
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
    }] ++ headers2

    let headers3a = map (linkSectionHeader headers3) headers3
    let headers4 = getSectionHeaderNameOffsets headers3a shstrtab

    let e_shnum = fromIntegral (length headers3) :: Int16
    let e_shstrndx = fromIntegral (getSectionHeaderIndex headers4 ".shstrtab")
                                  :: Int16

    let header = [0x7f, 0x45, 0x4c, 0x46,          -- magic
                  0x02,                            -- 64-bit
                  0x01,                            -- little endian
                  0x01,                            -- ELF version
                  0x00,                            -- System V ABI
                  0x00,                            -- ABI version
                  0x00, 0x00, 0x00, 0x00,
                  0x00, 0x00, 0x00,                -- padding
                  0x01, 0x00,                      -- relocatable
                  0x3e, 0x00,                      -- e_machine
                  0x01, 0x00, 0x00, 0x00,          -- e_version
                  0x00, 0x00, 0x00, 0x00,          -- entry point?
                  0x00, 0x00, 0x00, 0x00,
                  0x00, 0x00, 0x00, 0x00,          -- e_phoff
                  0x00, 0x00, 0x00, 0x00] ++
                  renderInt64(e_shoff) ++          -- e_shoff
                 [0x00, 0x00, 0x00, 0x00] ++       -- e_flags
                  renderInt16(e_ehsize) ++         -- e_ehsize
                 [0x00, 0x00,                      -- e_phentsize?
                  0x00, 0x00] ++                   -- e_phnum
                  renderInt16(e_shentsize) ++      -- e_shentsize
                  renderInt16(e_shnum) ++          -- e_shnum
                  renderInt16(e_shstrndx) :: [Int] -- e_shstrndx

    let renderedSectionHeaders = map renderSectionHeader headers4

    let rendered = header ++
                   (concat renderedSectionHeaders) ++
                   renderSectionsData(headers4)

    --putStr "\n"
    BL.putStr (toByteString rendered)

showSection :: Section -> [Char]
showSection s = "[" ++ (kind s) ++ "]\n" ++
                (intercalate "\n" (map showInstruction (instructions s)))

showSections :: [Section] -> [Char]
showSections s = "--SECTIONS:-----------------------\n\n" ++
                 (intercalate "\n" (map showSection s))

showInstruction :: Instruction -> [Char]
showInstruction i =
    (intercalate "," (labels i)) ++
    (if (null (labels i)) then "" else ":\n") ++
    "  " ++ (show (instructionOffset i)) ++ ": " ++ (command i) ++ "\n  " ++
    (intercalate "\n  " (map showOperand (operands i))) ++ "\n"

showOperand :: Operand -> [Char]
showOperand o = show (operandOffset o) ++ ": " ++ text o
