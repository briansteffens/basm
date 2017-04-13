module Parser where

import Data.Binary
import Data.Char
import Data.Int
import Data.List
import Data.Maybe
import qualified Text.Read as TR

import Shared
import Definitions
import Tokenizer


data Section = Section {
    name     :: String,
    contents :: [Line]
}


data Error = Error     String
           | LineError Line   String


-- Check if a token is quoted.
isQuoted :: Token -> Bool
isQuoted (Unquoted _) = False
isQuoted (Quoted   _) = True


-- Parse a line as a section directive, returning the section name if it is
-- a section directive and Nothing if not.
parseSectionName :: Line -> (Maybe String, [Error])
parseSectionName line = do
    let firstParam = head (tokens line)
    let secondParam = last (tokens line)

    let isSection = firstParam == Unquoted "section"
    let correctParamCount = length (tokens line) == 2
    let isValid = correctParamCount && not (isQuoted secondParam)

    let errors = if not isSection then [] else if isValid
                     then []
                     else [LineError line "Invalid syntax"]

    let ret = if isSection && correctParamCount
                  then Just (tokenString secondParam)
                  else Nothing

    (ret, errors)


-- Break a list into 3 chunks based on a predicate.
doubleBreak :: (a -> Bool) -> [a] -> ([a], [a], [a])
doubleBreak predicate list = do
    let (before, remaining) = break predicate list
    let (current, after) = break predicate (tail remaining)

    if null remaining
        then (before, [], [])
        else (before, [head remaining] ++ current, after)


-- Divide a list of lines into sections.
divideSections :: [Line] -> ([Section], [Error])
divideSections []    = ([], [])
divideSections lines = do
    let isSection l = head (tokens l) == Unquoted "section"
    let (before, current, after) = doubleBreak isSection lines

    let sectionLine = head current

    let outsideSectionErrors = if null before then []
                               else [LineError sectionLine
                                               "Code found outside section"]

    let (sectionName, parseErrors) = parseSectionName sectionLine

    let section = Section {
        name     = fromJust sectionName,
        contents = tail current
    }

    let errors = outsideSectionErrors ++ parseErrors

    let (recurSections, recurErrors) = divideSections after
    ([section] ++ recurSections, errors ++ recurErrors)


-- Get the first token from a line and upper-case it.
firstToken :: Line -> String
firstToken line = map toUpper (tokenString (head (tokens line)))


-- Split a list on some predicate.
split :: (a -> Bool) -> [a] -> [[a]]
split p []   = []
split p list = do
    let (current, remaining) = break p list
    let recur = if null remaining then [] else split p (tail remaining)
    [current] ++ recur


-- Split a list of tokens on commas.
splitOperands :: [Token] -> [[Token]]
splitOperands t = split (== ControlChar ',') t


makePart :: Maybe Registers -> Maybe Integer -> String -> OperandPart
makePart (Just r) _        _ = RegisterPart r
makePart Nothing  (Just n) _ = NumericPart n
makePart Nothing  Nothing  s = SymbolPart s


parsePart :: Token -> OperandPart
parsePart (ControlChar c) = ControlPart c
parsePart (Quoted      s) = QuotedPart  s
parsePart (Unquoted    t) = do
    let parsedRegister = TR.readMaybe (map toUpper t) :: Maybe Registers
    let parsedInt = TR.readMaybe t :: Maybe Integer

    makePart parsedRegister parsedInt t


-- Check if an integer value can fit in an Int8
fitsInt8 :: Integer -> Bool
fitsInt8 v = v >= fromIntegral (minBound :: Int8) &&
             v <= fromIntegral (maxBound :: Int8)


-- Check if an integer value can fit in an Int32
fitsInt32 :: Integer -> Bool
fitsInt32 v = v >= fromIntegral (minBound :: Int32) &&
              v <= fromIntegral (maxBound :: Int32)


parseDisplacement :: Integer -> Displacement
parseDisplacement i
    | fitsInt8  i = Displacement8  (fromIntegral i)
    | fitsInt32 i = Displacement32 (fromIntegral i)
    | otherwise   = error("Invalid displacement value")


parseScale :: Integer -> Scale
parseScale 1 = NoScale
parseScale 2 = Scale2
parseScale 4 = Scale4
parseScale 8 = Scale8
parseScale _ = error("Invalid scale value")


-- Parse an operand from a list of tokens.
parseOperand :: Line -> Command -> [OperandPart] -> ([Operand], [Error])
parseOperand _ _   []               = ([], [])
parseOperand _ _   [RegisterPart r] = ([Register r], [])

parseOperand _ _   [NumericPart  n] = do
    let bytes = takeWhile (/= 0) (toBytes n)
    let lit = if null bytes then [0x00] else bytes
    ([Immediate (Literal lit)], [])

parseOperand _ cmd [SymbolPart   s]
    | elem cmd jumpCommands         = ([Relative  (Symbol DWORD s)], [])
    | otherwise                     = ([Immediate (Symbol QWORD s)], [])

-- [rax]
parseOperand _ _ [ControlPart '[', RegisterPart b, ControlPart ']'] =
    ([Address b NoScale NoRegister NoDisplacement], [])

-- [rax+rbx]
parseOperand _ _ [ControlPart '[', RegisterPart b, ControlPart '+',
                  RegisterPart i, ControlPart ']'] =
    ([Address b NoScale i NoDisplacement], [])

-- [rax+8]
parseOperand _ _ [ControlPart '[', RegisterPart b, ControlPart '+',
                  NumericPart n, ControlPart ']'] =
    ([Address b NoScale NoRegister (parseDisplacement n)], [])

-- [rax-8]
parseOperand _ _ [ControlPart '[', RegisterPart b, ControlPart '-',
                  NumericPart n, ControlPart ']'] =
    ([Address b NoScale NoRegister (parseDisplacement (-1 * n))], [])

-- [rax*8+rbx]
parseOperand _ _ [ControlPart '[', RegisterPart i, ControlPart '*',
                  NumericPart s, ControlPart '+', RegisterPart b,
                  ControlPart ']'] =
    ([Address b (parseScale s) i NoDisplacement], [])

-- Catch-all for expressions to be parsed later
parseOperand _ _ parts = ([Expression parts], [])


-- Concatenate the members of a 2-tuple of lists.
concatTuple :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatTuple l r = (fst l ++ fst r, snd l ++ snd r)


-- Concatenate the members of a 3-tuple of lists.
concatTuple3 :: ([a], [b], [c]) -> ([a], [b], [c]) -> ([a], [b], [c])
concatTuple3 (la, lb, lc) (ra, rb, rc) = (la ++ ra, lb ++ rb, lc ++ rc)


parseSize :: String -> Maybe Size
parseSize "BYTE"  = Just BYTE
parseSize "WORD"  = Just WORD
parseSize "DWORD" = Just DWORD
parseSize "QWORD" = Just QWORD
parseSize _       = Nothing


-- Remove and parse any size hints, returning both size hints and the rest of
-- the token stream.
extractSizes :: [Token] -> ([Size], [Token])
extractSizes [] = ([], [])
extractSizes (Unquoted cur:rest) = do
    let size = case parseSize (map toUpper cur) of Nothing -> []
                                                   Just s  -> [s]

    let token = if null size then [Unquoted cur] else []

    concatTuple (size, token) (extractSizes rest)
extractSizes (cur:rest) = concatTuple ([], [cur]) (extractSizes rest)


-- If an instruction contained size hints, make sure they all match.
resolveSize :: Line -> [Size] -> (Size, [Error])
resolveSize _    []           = (NoSize, [])
resolveSize line (first:rest) = do
    let allMatch = all (== first) rest
    let errors = if allMatch then [] else [LineError line "Size hint mismatch"]
    (first, errors)


-- Convert a character to a byte in ASCII format.
asciiByte :: Char -> Word8
asciiByte c = fromIntegral (ord c)


-- Parse operand parts for a DB pseudo-instruction, which converts everything
-- to bytes.
parseDbOperand :: [OperandPart] -> [Word8]
parseDbOperand [NumericPart n] = [fromIntegral n :: Word8]
parseDbOperand [QuotedPart  s] = map asciiByte s


-- Parse operand parts into AST operands.
parseOperands :: Line -> Command -> [[OperandPart]] -> ([Operand], [Error])
parseOperands l DB  p = ([Immediate (Literal (concat (map parseDbOperand p)))],
                         [])
parseOperands l cmd p = do
    foldl concatTuple ([], []) (map (parseOperand l cmd) p)


-- Parse an instruction if possible.
parseLine :: Line -> (Maybe Instruction, [Error])
parseLine line = do
    let commandParsed = TR.readMaybe (firstToken line) :: Maybe Command

    let command = fromJust commandParsed
    let commandErr = if commandParsed /= Nothing
                         then []
                         else [LineError line "Unrecognized command."]

    let (sizes, toks) = extractSizes (tail (tokens line))
    let (size, sizeErr) = resolveSize line sizes

    let operandTokens = splitOperands toks
    let operandParts = map (map parsePart) operandTokens
    let (operands, operandErr) = parseOperands line command operandParts

    let inst = Instruction {
        source     = code line,
        labelNames = labels line,
        sizeHint   = size,
        command    = command,
        operands   = operands
    }

    let err = commandErr ++ sizeErr ++ operandErr

    (if null err then Just inst else Nothing, err)


parseSection :: Section -> (CodeSection, [Error])
parseSection sec = do
    let parsed = map parseLine (contents sec)
    let err = concat (map snd parsed)
    let insts = justList (map fst parsed)
    let ret = CodeSection {
        sectionName = name sec,
        instructions = insts
    }
    (ret, err)


readSymbolType :: String -> SymbolType
readSymbolType st
    | s == "FUNCTION" = STT_FUNC
    | otherwise       = STT_NOTYPE
    where s = map toUpper st


parseGlobalOperand :: [Token] -> Directive
parseGlobalOperand [Unquoted n]             = Global STT_NOTYPE n
parseGlobalOperand [Unquoted n, Unquoted t] = Global (readSymbolType t)
                                                     (init n)


parseExternOperand :: [Token] -> Directive
parseExternOperand [Unquoted n] = Extern n


parseDirective :: Line -> ([Directive], [Error])
parseDirective Line { tokens=(Unquoted "global":params) } = do
    let parts = splitOperands params
    (map parseGlobalOperand parts, [])
parseDirective Line { tokens=(Unquoted "extern":params) } = do
    let parts = splitOperands params
    (map parseExternOperand parts, [])
parseDirective _ = ([], [])


extractDirectives :: [Line] -> ([Directive], [Line], [Error])
extractDirectives [] = ([], [], [])
extractDirectives (line:rest) = do
    let (directives, errors) = parseDirective line
    let success = null errors && not (null directives)

    let lines = if success then []
                           else [line]

    let recur = extractDirectives rest
    concatTuple3 (directives, lines, errors) recur


-- Convert code into an AST.
parse :: String -> ([CodeSection], [Directive], [Error], String)
parse src = do
    let toks = tokenize src

    let debug = "Tokens: " ++ intercalate "\n" (map showLine toks)

    let (directives, linesLeft, directiveErrors) = extractDirectives toks

    let (sections, divideErrors) = divideSections linesLeft

    let parsed = map parseSection sections

    let codeSections = map fst parsed
    let err = directiveErrors ++ divideErrors ++ concat (map snd parsed)

    (codeSections, directives, err, debug)


-- Concatenate the members of a tuple of lists.
concatMaybeTuple :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatMaybeTuple l r = (fst l ++ fst r, snd l ++ snd r)


justList :: [Maybe a] -> [a]
justList []             = []
justList (Nothing:rest) = justList rest
justList (Just x:rest)  = [x] ++ justList rest


showCodeSection :: CodeSection -> String
showCodeSection sec =
    "section " ++ (sectionName sec) ++ "\n" ++
        intercalate "\n" (map showInstruction (instructions sec))


showError :: Error -> String
showError (Error       s) = "ERROR: " ++ s
showError (LineError l s) = "ERROR: " ++ s ++ "\n  line " ++ showLine l


showSection :: Section -> String
showSection sec =
    "section " ++ (name sec) ++ "\n" ++
        intercalate "\n" (map showLine (contents sec)) ++ "\n"


showLine :: Line -> String
showLine l = do
    let lbls = labels l

    "[" ++ show (lineNumber l) ++ "] " ++ code l ++ "  " ++
        (if null lbls then "" else (intercalate "," lbls ++ ":\n  ")) ++ "|" ++
        intercalate "| |" (map showToken (tokens l)) ++ "|"


showToken :: Token -> String
showToken (Unquoted    s) = s
showToken (Quoted      s) = s
showToken (ControlChar c) = [c]
