module Elf where

import Data.Binary
import Data.Bits
import Data.Char
import Data.Int
import Data.List
import qualified Data.ByteString.Lazy as B

import Debug.Trace (trace)

import Shared
import qualified Definitions as D
import qualified Encoder as E


-- The size of the ELF header in bytes (e_ehsize)
elfHeaderSize = 64


-- The size of each section header table entry (e_shentsize)
sectionHeaderSize = 64


-- Each section header's offset should be aligned to this value
sectionHeaderAlignment = 16


-- Section header type
data ShType = SHT_NULL
            | SHT_PROGBITS
            | SHT_SYMTAB
            | SHT_STRTAB
            | SHT_RELA
            deriving Show


renderShType :: ShType -> Word32
renderShType SHT_NULL     = 0
renderShType SHT_PROGBITS = 1
renderShType SHT_SYMTAB   = 2
renderShType SHT_STRTAB   = 3
renderShType SHT_RELA     = 4


-- Section header flag field
data ShFlags = SHF_NONE
             | SHF_ALLOC_WRITE
             | SHF_ALLOC_EXEC
             deriving Show


renderShFlags :: ShFlags -> Word64
renderShFlags SHF_NONE        = 0
renderShFlags SHF_ALLOC_EXEC  = 6
renderShFlags SHF_ALLOC_WRITE = 3


-- Symbol table entry visibility field (st_other)
data SymbolVisibility = STV_DEFAULT
                      deriving Show


renderSymbolVisibility :: SymbolVisibility -> Word8
renderSymbolVisibility STV_DEFAULT = 0


-- Symbol table entry binding field (part of st_info)
data SymbolBinding = STB_LOCAL
                   | STB_WEAK
                   | STB_GLOBAL
                   deriving (Show, Eq, Ord)


renderSymbolBinding :: SymbolBinding -> Word8
renderSymbolBinding STB_LOCAL  = 0
renderSymbolBinding STB_GLOBAL = 1
renderSymbolBinding STB_WEAK   = 2


-- Defines what section (if any) the symbol refers to (st_shndx)
data SymbolRelation = RelationUndefined
                    | RelationAbsolute
                    | RelatedSection    String


-- An entry in the symtab
data Symbol = Symbol {
    symbolName :: String,
    symbolType :: D.SymbolType,
    binding    :: SymbolBinding,
    visibility :: SymbolVisibility,
    relation   :: SymbolRelation,
    value      :: [Word8],
    size       :: Word64
}


-- The contents of a section
data Contents = NoContents
              | ProgBitsContents E.EncodedSection
              | RelaContents     RelocationSection
              | StrTabContents   [String]
              | SymTabContents   [Symbol]


-- An ELF section
data Section = Section {
    sectionName :: String,
    contents    :: Contents
}


-- An entry in the section header table, describing a section
data SectionHeader = SectionHeader {
    sh_name         :: Word32,
    sh_type         :: ShType,
    sh_flags        :: ShFlags,
    sh_addr         :: Word64,
    sh_offset       :: Word64,
    sh_size         :: Word64,
    sh_link         :: Word32,
    sh_info         :: Word32,
    sh_addralign    :: Word64,
    sh_entsize      :: Word64
}


-- The type of a relocation table entry
data RelocationType = R_X86_64_64
                    | R_X86_64_PC32
                    | R_X86_64_32S


-- The contents of a relocation table
data RelocationSection = RelocationSection {
    sourceSection :: E.EncodedSection,
    relocations   :: [Relocation]
}


-- An entry in a relocation table
data Relocation
    = LocalRelocation {
        sourceOffset   :: E.NamedOffset,
        targetSection  :: E.EncodedSection,
        targetLabel    :: E.Label,
        relocationType :: RelocationType
    }
    | ExternRelocation {
        sourceOffset  :: E.NamedOffset,
        externName    :: String
    }


-- Convert a RelocationType into a 4-byte format for a rela tab
renderRelocationType :: RelocationType -> [Word8]
renderRelocationType R_X86_64_64   = [0x01, 0x00, 0x00, 0x00]
renderRelocationType R_X86_64_PC32 = [0x02, 0x00, 0x00, 0x00]
renderRelocationType R_X86_64_32S  = [0x0b, 0x00, 0x00, 0x00]


-- Find a label and its section by label name
findLabel :: [E.EncodedSection] -> String -> Maybe (E.EncodedSection, E.Label)
findLabel []             _    = Nothing
findLabel (section:rest) name =
    case find (\l -> (E.label l) == name) (E.labels section) of
        Just l  -> Just (section, l)
        Nothing -> findLabel rest name


-- Generate relocations from sections
generateRelocations :: [String] -> [E.EncodedSection] -> [Section]
generateRelocations externs sections = do
    let makeRelo offset = do
        let (ts, l) = case findLabel sections (E.name offset) of
                     Just (ts, l) -> (ts, l)
                     Nothing      -> error ("Label " ++ (E.name offset) ++
                                            " not found")

        let reloType = if E.offsetType offset == E.OffsetImmediate
                           then R_X86_64_64
                           else R_X86_64_32S

        let localRelo = LocalRelocation {
            sourceOffset   = offset,
            targetSection  = ts,
            targetLabel    = l,
            relocationType = reloType
        }

        let externRelo = ExternRelocation {
            sourceOffset = offset,
            externName   = E.name offset
        }

        let isExtern = elem (E.name offset) externs
        if isExtern then externRelo else localRelo

    let handleSection section = do
        let relos = map makeRelo (E.symbols section)

        case length relos of
            0 -> []
            _ -> [Section {
                   sectionName = ".rela" ++ D.sectionName (E.section section),
                   contents = RelaContents (RelocationSection {
                         sourceSection = section,
                         relocations = relos
                   })}]

    concat (map handleSection sections)


-- Find the index of an element in a list by predicate
indexOf :: (a -> Bool) -> [a] -> Maybe Int
indexOf p all = do
    let broken = break p all
    case (length (snd broken)) of
        0 -> Nothing
        _ -> Just (length (fst broken))


-- Look up the index of a section by name
sectionIndex :: String -> [Section] -> Int
sectionIndex section all = do
    case indexOf (\s -> section == sectionName s) all of
        Nothing -> error("Section " ++ section ++ " not found")
        Just i  -> i


-- Get the index of the symbol representing a section by section name
sectionSymbolIndex :: [Symbol] -> String -> Int
sectionSymbolIndex symbols sectionName = do
    let test sym = case relation sym of
                  RelatedSection s -> s == sectionName
                  _                -> False

    case indexOf test symbols of
        Nothing -> error("Section symbol " ++ sectionName ++ " not found")
        Just i  -> i


-- Get the index of a symbol by its name
symbolIndex :: [Symbol] -> String -> Int
symbolIndex symbols name = do
    let test sym = (symbolName sym) == name
    case indexOf test symbols of
        Nothing -> error("Symbol " ++ name ++ " not found")
        Just i  -> i


-- Check if a directive is a global with the given name.
matchGlobalDirective :: String -> D.Directive -> Bool
matchGlobalDirective search (D.Global _ name) = name == search
matchGlobalDirective _      _                 = False


-- Generate a symtab including a null symbol, filename symbol, symbols for
-- each PROGBITS section, and symbols for each label
generateSymTab :: [E.EncodedSection] -> String -> [D.Directive] -> Section
generateSymTab sections filename directives = do
    -- The null symbol at index 0 in every file's symtab
    let nullSymbol = Symbol {
        symbolName = "",
        symbolType = D.STT_NOTYPE,
        binding    = STB_LOCAL,
        visibility = STV_DEFAULT,
        relation   = RelationUndefined,
        value      = [0, 0, 0, 0, 0, 0, 0, 0],
        size       = 0
    }

    -- This symbol represents the object file itself
    let fileSymbol = Symbol {
        symbolName = filename,
        symbolType = D.STT_FILE,
        binding    = STB_LOCAL,
        visibility = STV_DEFAULT,
        relation   = RelationAbsolute,
        value      = [0, 0, 0, 0, 0, 0, 0, 0],
        size       = 0
    }

    -- Each PROGBITS section should have a symbol for it
    let sectionSymbol sec = Symbol {
        symbolName = "",
        symbolType = D.STT_SECTION,
        binding    = STB_LOCAL,
        visibility = STV_DEFAULT,
        relation   = RelatedSection (D.sectionName (E.section sec)),
        value      = [0, 0, 0, 0, 0, 0, 0, 0],
        size       = 0
    }

    let sectionSymbols = map sectionSymbol sections

    -- Each label across all PROGBITS sections should have a symbol for it
    let labelSection sec = do

        let labelSymbol label = do
            let directive = find (matchGlobalDirective (E.label label))
                                 directives

            let symType = case directive of
                              Just (D.Global t _) -> t
                              otherwise           -> D.STT_NOTYPE

            let symBinding = case directive of
                                 Just _    -> STB_GLOBAL
                                 otherwise -> STB_LOCAL

            Symbol {
                symbolName = E.label label,
                symbolType = symType,
                binding    = symBinding,
                visibility = STV_DEFAULT,
                relation   = RelatedSection (D.sectionName (E.section sec)),
                value      = toBytes (E.labelOffset label),
                size       = 0
            }

        map labelSymbol (E.labels sec)

    let labels = concat (map labelSection sections)

    -- Extern directives should have symbols for them
    let externSymbol d = do
        case d of
            D.Extern name -> [Symbol {
                symbolName = name,
                symbolType = D.STT_NOTYPE,
                binding    = STB_GLOBAL,
                visibility = STV_DEFAULT,
                relation   = RelationUndefined,
                value      = [0, 0, 0, 0, 0, 0, 0, 0],
                size       = 0
            }]
            _ -> []

    let externs = concat (map externSymbol directives)

    let sortable = labels ++ externs
    let sorted = sortBy (\l r -> compare (binding l) (binding r)) sortable

    Section {
        sectionName = ".symtab",
        contents    = SymTabContents ([nullSymbol, fileSymbol] ++
                                      sectionSymbols ++
                                      sorted)
    }


-- Convert a relocation entry into bytes
renderRelocation :: [Section] -> [Symbol] -> Relocation -> [Word8]

renderRelocation all symbols relo@(LocalRelocation _ _ _ _) = do
    let targetName = D.sectionName (E.section (targetSection relo))
    let targetIndex = sectionSymbolIndex symbols targetName
    let labelOffset = E.labelOffset (targetLabel relo)

    toBytes (E.offset (sourceOffset relo)) ++
        renderRelocationType (relocationType relo) ++
        toBytes (fromIntegral targetIndex :: Word32) ++
        toBytes (fromIntegral labelOffset :: Int64)

renderRelocation all symbols relo@(ExternRelocation _ _) = do
    let targetName = externName relo
    let targetIndex = symbolIndex symbols targetName

    toBytes (E.offset (sourceOffset relo)) ++
        renderRelocationType R_X86_64_PC32 ++
        toBytes (fromIntegral targetIndex :: Word32) ++
        toBytes (fromIntegral (-4) :: Int64) -- TODO: why -4?


-- Convert a list of relocation table entries into bytes
renderRelocations :: [Section] -> [Relocation] -> [Word8]
renderRelocations all relos = do
    let symtab = getSection all ".symtab"
    let symbols = case contents symtab of
                 SymTabContents c -> c
                 _                -> error("No symtab present when " ++
                                           "rendering symtabs")

    concat (map (renderRelocation all symbols) relos)


-- Get a list of all extern names
getExternNames :: [D.Directive] -> [String]
getExternNames [] = []
getExternNames (D.Extern name:xs) = [name] ++ getExternNames xs
getExternNames (_            :xs) = getExternNames xs


-- Generate a strtab from a list of code sections and a filename
generateStrTab :: [D.Directive] -> [E.EncodedSection] -> String -> Section
generateStrTab directives sections filename = do
    let allExterns = getExternNames directives
    let allLabels = map E.label (concat (map E.labels sections))
    Section {
        sectionName = ".strtab",
        contents    = StrTabContents (["", filename] ++ allLabels ++
                                                        allExterns)
    }


-- Generate a section header strtab containing the names of all existing
-- sections except the null section
generateShStrTab :: [Section] -> Section
generateShStrTab sections =
    Section {
        sectionName = ".shstrtab",
        contents    = StrTabContents ((map sectionName sections) ++
                                      [".shstrtab"])
    }


-- Look up a section by name; fails if it can't be found
getSection :: [Section] -> String -> Section
getSection sections search =
    case find (\s -> sectionName s == search) sections of
        Nothing -> error("Section " ++ search ++ " not found.")
        Just s  -> s


-- Calculate the combined lengths of an array of strings from a string table,
-- assuming ASCII and counting null-termination characters
stringLengths :: [String] -> Int
stringLengths strings = foldl (+) 0 (map succ (map length strings))


-- Look up the index of a string within a specific strtab
stringIndex :: [Section] -> String -> String -> Word32
stringIndex sections secName search = do
    let c = case contents (getSection sections secName) of
           StrTabContents c -> c
           _                -> error("Wrong section type")

    let broken = break (\s -> s == search) c

    case (length (snd broken)) of
        0 -> error("String " ++ search ++ " not found in " ++ secName)
        _ -> fromIntegral (stringLengths (fst broken)) :: Word32


shNameIndex :: [Section] -> Section -> Word32
shNameIndex all sec = stringIndex all ".shstrtab" (sectionName sec)


relaEntSize = 24
symbolEntSize = 24


-- Generate a section header for a given section
sh :: [Section] -> Section -> Contents -> Word64 -> SectionHeader

-- The null section header (index 0)
sh _ _ NoContents _ = SectionHeader {
    sh_name      = 0,
    sh_type      = SHT_NULL,
    sh_flags     = SHF_NONE,
    sh_addr      = 0,
    sh_offset    = 0,
    sh_size      = 0,
    sh_link      = 0,
    sh_info      = 0,
    sh_addralign = 0,
    sh_entsize   = 0
}

sh all sec (ProgBitsContents enc) offset = SectionHeader {
    sh_name      = shNameIndex all sec,
    sh_type      = SHT_PROGBITS,
    -- TODO: set flags correctly
    sh_flags     = if (sectionName sec) == ".data" then SHF_ALLOC_WRITE
                                                   else SHF_ALLOC_EXEC,
    sh_addr      = 0,
    sh_offset    = offset,
    sh_size      = fromIntegral (length (E.bytes enc)) :: Word64,
    sh_link      = 0,
    sh_info      = 0,
    sh_addralign = if (sectionName sec) == ".data" then 4 else 16,
    sh_entsize   = 0
}

sh all sec (StrTabContents c) offset = SectionHeader {
    sh_name      = shNameIndex all sec,
    sh_type      = SHT_STRTAB,
    sh_flags     = SHF_NONE,
    sh_addr      = 0,
    sh_offset    = offset,
    sh_size      = fromIntegral (stringLengths c) :: Word64,
    sh_link      = 0,
    sh_info      = 0,
    sh_addralign = 1,
    sh_entsize   = 0
}

sh all sec (SymTabContents symbols) offset = SectionHeader {
    sh_name      = shNameIndex all sec,
    sh_type      = SHT_SYMTAB,
    sh_flags     = SHF_NONE,
    sh_addr      = 0,
    sh_offset    = offset,
    sh_size      = fromIntegral (symbolEntSize * (length symbols)) :: Word64,
    sh_link      = fromIntegral (sectionIndex ".strtab" all) :: Word32,
    sh_info      = fromIntegral (length (fst (break
                       (\s -> binding s == STB_GLOBAL) symbols))) :: Word32,
    sh_addralign = 8,
    sh_entsize   = fromIntegral symbolEntSize :: Word64
}

sh all sec (RelaContents r) offset = SectionHeader {
    sh_name      = shNameIndex all sec,
    sh_type      = SHT_RELA,
    sh_flags     = SHF_NONE,
    sh_addr      = 0,
    sh_offset    = offset,
    sh_size      = fromIntegral (relaEntSize * (length (relocations r)))
                                :: Word64,
    sh_link      = fromIntegral (sectionIndex ".symtab" all) :: Word32,
    sh_info      = fromIntegral (sectionIndex
                     (D.sectionName (E.section (sourceSection r))) all)
                     :: Word32,
    sh_addralign = 8,
    sh_entsize   = fromIntegral relaEntSize :: Word64
}


-- Align value to the nearest alignment by rounding up
align :: Int -> Int -> Int
align value alignment
    | r == 0    = value
    | otherwise = value + (alignment - r)
    where r = mod value alignment


-- Generate a list of section headers for a list of sections
sectionHeaders :: [Section] -> [Section] -> Int -> [SectionHeader]
sectionHeaders _ [] _ = []
sectionHeaders all (current:rest) offset = do
    let currentSh = sh all current (contents current)
                       (fromIntegral offset :: Word64)
    let nextOffset = align (offset + fromIntegral (sh_size currentSh) :: Int)
                           sectionHeaderAlignment
    [currentSh] ++ sectionHeaders all rest nextOffset


-- Render a section header into bytes
renderSectionHeader :: SectionHeader -> [Word8]
renderSectionHeader sh = toBytes (sh_name sh) ++
                         toBytes (renderShType (sh_type sh)) ++
                         toBytes (renderShFlags (sh_flags sh)) ++
                         toBytes (sh_addr sh) ++
                         toBytes (sh_offset sh) ++
                         toBytes (sh_size sh) ++
                         toBytes (sh_link sh) ++
                         toBytes (sh_info sh) ++
                         toBytes (sh_addralign sh) ++
                         toBytes (sh_entsize sh)


renderSymbolType :: D.SymbolType -> Word8
renderSymbolType D.STT_NOTYPE  = 0
renderSymbolType D.STT_FUNC    = 2
renderSymbolType D.STT_SECTION = 3
renderSymbolType D.STT_FILE    = 4


-- Combine symbol binding and type into the sh_info field
renderSymTabInfo :: SymbolBinding -> D.SymbolType -> Word8
renderSymTabInfo b t = (shiftL (renderSymbolBinding b) 4) +
                       (renderSymbolType t)


-- Render a symbol relation
renderRelation :: [Section] -> SymbolRelation -> Word16
renderRelation _   RelationUndefined    = 0
renderRelation _   RelationAbsolute     = 65521
renderRelation all (RelatedSection sec) = fromIntegral (sectionIndex sec all)


-- Render a symtab entry
renderSymbol :: [Section] -> Symbol -> [Word8]
renderSymbol sections sym =
    toBytes (stringIndex sections ".strtab" (symbolName sym)) ++
        [renderSymTabInfo (binding sym) (symbolType sym),
         renderSymbolVisibility (visibility sym)] ++
        toBytes (renderRelation sections (relation sym)) ++
        value sym ++
        toBytes (size sym)


-- Render a section's contents
renderContents :: [Section] -> Contents -> [Word8]
renderContents _ NoContents = []
renderContents _ (ProgBitsContents enc) = (E.bytes enc)
renderContents sections (RelaContents rel) =
    renderRelocations sections (relocations rel)

renderContents _ (StrTabContents strings) = do
    let renderString s = map fromIntegral (map ord s ++ [0]) :: [Word8]
    concat (map renderString strings)

renderContents sections (SymTabContents symbols) =
    concat (map (renderSymbol sections) symbols)


-- Combine section contents, inserting padding as necessary to match the
-- sh_offset fields in each section's header
combineContents :: [(SectionHeader, [Word8])] -> Int -> [Word8]
combineContents []                      _      = []
combineContents ((sh, bytes):remaining) offset = do
    let targetAlignment = fromIntegral (sh_offset sh) :: Int
    let paddingBytes = targetAlignment - offset
    let padding = replicate paddingBytes 0 :: [Word8]
    let paddedBytes = padding ++ bytes
    let nextOffset = offset + length paddedBytes
    paddedBytes ++ combineContents remaining nextOffset


-- Encode and assemble an ELF file.
assemble :: [D.Section] -> [D.Directive] -> (B.ByteString, String)
assemble codeSections directives = do
    -- Encode instructions
    let progBitsEncoded = map E.encodeSection codeSections


    -- Generate strtab
    let filename = "file.asm"
    let strTab = generateStrTab directives progBitsEncoded filename


    -- Generate null section
    let nullSection = Section {
        sectionName = "",
        contents = NoContents
    }


    -- Generate progbits sections
    let progBitsSection c = Section {
        sectionName = D.sectionName (E.section c),
        contents = ProgBitsContents c
    }
    let progBitsSections = map progBitsSection progBitsEncoded


    -- Generate symtab
    let symTab = generateSymTab progBitsEncoded filename directives
    let symbols = case contents symTab of
                 SymTabContents c -> c
                 _                -> error("Not a symtab")


    -- Generate relocations
    let externNames = getExternNames directives
    let relocationSections = generateRelocations externNames progBitsEncoded
    let sectionsProgReloSymStr = [nullSection] ++ progBitsSections ++
                                 relocationSections ++ [symTab, strTab]


    -- Generate shstrtab
    let shStrTab = generateShStrTab sectionsProgReloSymStr
    let sections = sectionsProgReloSymStr ++ [shStrTab]


    -- Generate section headers
    let sectionsOffset = elfHeaderSize + sectionHeaderSize * length sections
    let shs = sectionHeaders sections sections sectionsOffset


    -- Generate ELF header
    let e_shnum = fromIntegral (length shs) :: Word16
    let e_shstrndx = fromIntegral (sectionIndex ".shstrtab" sections) :: Word16

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
                  0x00, 0x00, 0x00, 0x00,
                  0x40, 0x00, 0x00, 0x00,                      -- e_shoff
                  0x00, 0x00, 0x00, 0x00,
                  0x00, 0x00, 0x00, 0x00,                      -- e_flags
                  0x40, 0x00,                                  -- e_ehsize
                  0x00, 0x00,                                  -- e_phentsize?
                  0x00, 0x00,                                  -- e_phnum
                  0x40, 0x00] ++                               -- e_shentsize
                  toBytes e_shnum ++                           -- e_shnum
                  toBytes e_shstrndx                           -- e_shstrndx


    -- Render sections
    let allContents = map (renderContents sections) (map contents sections)


    -- Construct object file
    let out = header ++
              concat (map renderSectionHeader shs) ++
              combineContents (zip shs allContents) sectionsOffset


    let debug = "strtab: " ++ showStrTab strTab ++ "\n" ++
                show (length symbols) ++ "\n" ++
                intercalate "\n" (map showSymbol symbols) ++ "\n" ++
                intercalate "\n" (map showRelocationSection
                                      relocationSections) ++ "\n" ++
                "shstrtab: " ++ showStrTab shStrTab ++ "\n" ++
                intercalate "\n" (map showSectionHeader shs) ++ "\n" ++
                intercalate " " (map show out) ++ "\n"

    (B.pack out, debug)


showSymbolRelation :: SymbolRelation -> String
showSymbolRelation RelationUndefined  = "Undefined"
showSymbolRelation RelationAbsolute   = "Absolute"
showSymbolRelation (RelatedSection s) = "Section " ++ s


showSymbol :: Symbol -> String
showSymbol symbol = "symbol:\n" ++
    "  symbolName = " ++ symbolName symbol ++ "\n" ++
    "  symbolType = " ++ show (symbolType symbol) ++ "\n" ++
    "  binding    = " ++ show (binding symbol) ++ "\n" ++
    "  visibility = " ++ show (visibility symbol) ++ "\n" ++
    "  relation   = " ++ showSymbolRelation (relation symbol) ++ "\n" ++
    "  value      = " ++ intercalate ", " (map show (value symbol)) ++ "\n" ++
    "  size       = " ++ show (size symbol) ++ "\n"


showSectionHeader :: SectionHeader -> String
showSectionHeader sh = "SectionHeader:\n" ++
    "  sh_name      = " ++ (show (sh_name sh)) ++ "\n" ++
    "  sh_type      = " ++ (show (sh_type sh)) ++ "\n" ++
    "  sh_flags     = " ++ (show (sh_flags sh)) ++ "\n" ++
    "  sh_addr      = " ++ (show (sh_addr sh)) ++ "\n" ++
    "  sh_offset    = " ++ (show (sh_offset sh)) ++ "\n" ++
    "  sh_size      = " ++ (show (sh_size sh)) ++ "\n" ++
    "  sh_link      = " ++ (show (sh_link sh)) ++ "\n" ++
    "  sh_info      = " ++ (show (sh_info sh)) ++ "\n" ++
    "  sh_addralign = " ++ (show (sh_addralign sh)) ++ "\n" ++
    "  sh_entsize   = " ++ (show (sh_entsize sh)) ++ "\n"


showRelocationSection :: Section -> String
showRelocationSection sec = do
    let reloSec = case contents sec of
                 RelaContents r -> r
                 _              -> error("Expected RelaContents")
    D.sectionName (E.section (sourceSection reloSec)) ++ ":\n  " ++
        intercalate "\n  " (map showRelocation (relocations reloSec))


showRelocation :: Relocation -> String
showRelocation relo =
    E.name (sourceOffset relo) ++ ":" ++
    show (E.offset (sourceOffset relo)) ++ " -> " ++
    D.sectionName (E.section (targetSection relo)) ++ ":" ++
    E.label (targetLabel relo) ++ ":" ++
    show (E.labelOffset (targetLabel relo))


showStrTab tab = case contents tab of
    StrTabContents s -> intercalate " " s
    _                -> error("not a strtab")


showOffset (E.NamedOffset n o s _) = n ++ "=(" ++ show s ++ ") " ++ show o


showLabel (E.Label n o) = n ++ "=" ++ (show o)


showEnc e = show (E.bytes e) ++ "\n" ++
            (intercalate ", " (map showOffset (E.symbols e))) ++ "\n"++
            (intercalate ", " (map showLabel (E.labels e)))
