module Main where

import Data.Int
import Data.List
import Data.Char
import Data.Bits
import Data.Binary
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Debug.Trace (trace)

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

    let calculator = case command current of
         "mov"     -> calculateMovOffsets
         "syscall" -> calculateSysCallOffsets
         "db"      -> calculateDbOffsets
         x         -> error ("Unrecognized command: " ++ show x)

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
    let prefix = case (text (head (operands inst))) of
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
    let parsed = case (stringToInt (text source)) of Nothing -> 0
                                                     Just i  -> i
    prefix ++ renderInt parsed

toByteString :: [Int] -> BL.ByteString
toByteString input = BL.pack (map fromIntegral input)

renderSection :: Section -> [Int]
renderSection section = do
    concat (map renderInstruction (instructions section))

renderStrTab :: [[Char]] -> [Int]
renderStrTab strings = concat [(map ord s) ++ [0] | s <- strings]

renderInt :: (Binary a) => a -> [Int]
renderInt src = reverse [fromIntegral o :: Int | o <- BL.unpack (encode src)]

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

renderSectionData :: SectionHeader -> [Int]
renderSectionData header =
    case (length (sectionData header)) of
        0 -> []
        _ -> (sectionData header) ++ replicate (sectionPadding header) 0x00

renderSectionsData :: [SectionHeader] -> [Int]
renderSectionsData [] = []
renderSectionsData headers = concat (map renderSectionData headers)

data SymTabEntryType = STT_NOTYPE | STT_SECTION | STT_FILE

renderSymTabEntryType :: SymTabEntryType -> Int
renderSymTabEntryType t =
    case t of
        STT_NOTYPE  -> 0
        STT_SECTION -> 3
        STT_FILE    -> 4

data SymTabEntryVisibility = STV_DEFAULT

renderSymTabEntryVisibility :: SymTabEntryVisibility -> [Int]
renderSymTabEntryVisibility v =
    [case v of
        STV_DEFAULT -> 0]

data SymTabEntryBinding = STB_LOCAL | STB_GLOBAL

renderSymTabEntryBinding :: SymTabEntryBinding -> Int
renderSymTabEntryBinding b =
    case b of
        STB_LOCAL  -> 0
        STB_GLOBAL -> 1

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
        symTabBinding = case elem (fst label) globals of False -> STB_LOCAL
                                                         True  -> STB_GLOBAL,
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
    renderInt (fromIntegral (operandOffset (sourceOperand relo)) :: Int64) ++
    renderInt (fromIntegral (sectionIndex (targetSection relo)) :: Int32) ++
    renderInt (fromIntegral (sectionIndex (sourceSection relo)) :: Int32) ++
    renderInt (fromIntegral (instructionOffset (targetInstruction relo)) ::
                 Int64)

allLabels :: [Section] -> [[Char]]
allLabels sections = do
    let allInstructions = concat [instructions s | s <- sections]
    concat [labels i | i <- allInstructions]

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
    contents <- getContents

    let (tempSections, globals) = parse contents
    let sections = calculateSectionOffsets tempSections

    --putStr (showSections sections)

    let shRelo = reloHeaders (getRelocations sections)

    let e_ehsize = 64 :: Int16
    let e_shentsize = 64 :: Int16
    let e_shoff = 64 :: Int64

    let filename = "test2.asm"

    let strtab = ["", filename] ++ allLabels sections

    let userSectionHeaders = concat (map createSectionHeader sections)
    let dynamicSh = userSectionHeaders ++ shRelo

    let shstrtab = ["", ".shstrtab", ".symtab", ".strtab"] ++
                   [sectionName s | s <- dynamicSh]

    let renderedStrTab = renderStrTab(strtab)
    let renderedShStrTab = renderStrTab(shstrtab)

    let stNull = SymTabEntry {
        st_name  = "",
        symTabType  = STT_NOTYPE,
        symTabBinding = STB_LOCAL,
        st_other = STV_DEFAULT,
        st_shndx = 0,
        st_value = 0,
        st_size  = 0
    }

    let stFile = SymTabEntry {
        st_name  = filename,
        symTabType  = STT_FILE,
        symTabBinding = STB_LOCAL,
        st_other = STV_DEFAULT,
        st_shndx = 65521,
        st_value = 0,
        st_size  = 0
    }

    let stSections = sectionsToSymTab userSectionHeaders
    let stLabels = labelsToSymTab sections globals

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
        sh_info = fromIntegral ((length symtab) - 1) :: Int32,
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
    }] ++ shRelo

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
                  renderInt e_shoff ++             -- e_shoff
                 [0x00, 0x00, 0x00, 0x00] ++       -- e_flags
                  renderInt e_ehsize ++            -- e_ehsize
                 [0x00, 0x00,                      -- e_phentsize?
                  0x00, 0x00] ++                   -- e_phnum
                  renderInt e_shentsize ++         -- e_shentsize
                  renderInt e_shnum ++             -- e_shnum
                  renderInt e_shstrndx :: [Int]    -- e_shstrndx

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
