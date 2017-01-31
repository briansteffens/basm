module Main where

import Data.Char
import Data.List
import Data.Maybe
import Text.Read
import Debug.Trace

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


-- Parse an operand from a list of tokens.
parseOperand :: Line -> [Token] -> ([Operand], [Error])
parseOperand l [Unquoted t] = do
    let parsedRegister = readMaybe (map toUpper t) :: Maybe Registers
    let parsedInt = readMaybe t :: Maybe Integer

    let ret = if parsedRegister /= Nothing
                 then [Register (fromJust parsedRegister)]
                 else if parsedInt /= Nothing
                    then [Immediate (Literal (toBytes (fromJust parsedInt)))]
                    else []

    let err = if null ret then [LineError l ("Unrecognized operand: " ++ t)]
                          else []

    (ret, err)

parseOperand l _ = ([], [LineError l "Unrecognized operand"])


-- Concatenate the members of a tuple of lists.
concatTuple :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatTuple l r = (fst l ++ fst r, snd l ++ snd r)


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
extractSizes (cur:rest) = extractSizes rest


-- If an instruction contained size hints, make sure they all match.
resolveSize :: Line -> [Size] -> (Size, [Error])
resolveSize _    []           = (NoSize, [])
resolveSize line (first:rest) = do
    let allMatch = all (== first) rest
    let errors = if allMatch then [] else [LineError line "Size hint mismatch"]
    (first, errors)


-- Parse an instruction if possible.
parseLine :: Line -> (Maybe Instruction, [Error])
parseLine line = do
    let commandParsed = readMaybe (firstToken line) :: Maybe Command

    let commandErr = if commandParsed /= Nothing
                         then []
                         else [LineError line "Unrecognized command."]

    let (sizes, toks) = extractSizes (tail (tokens line))
    let (size, sizeErr) = resolveSize line sizes

    let operandTokens = splitOperands (tail (tokens line))
    let operandResults = map (parseOperand line) operandTokens
    let (operands, operandErr) = foldl concatTuple ([], []) operandResults

    let inst = Instruction {
        --source = "",
        labelNames = labels line,
        sizeHint = size,
        command = fromJust commandParsed,
        operands = operands
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


-- Convert code into an AST.
parse :: String -> ([CodeSection], [Error])
parse src = do
    let toks = tokenize src

    let (sections, errors) = divideSections toks

    let parsed = map parseSection sections

    let ret = map fst parsed
    let err = errors ++ concat (map snd parsed)

    (ret, err)


-- Concatenate the members of a tuple of lists.
concatMaybeTuple :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatMaybeTuple l r = (fst l ++ fst r, snd l ++ snd r)


justList :: [Maybe a] -> [a]
justList []             = []
justList (Nothing:rest) = justList rest
justList (Just x:rest)  = [x] ++ justList rest


main :: IO ()
main = do
    let (sections, errors) = parse testFile

    putStrLn ("\n" ++ (intercalate "\n\n" (map showCodeSection sections)))
    putStrLn "\n\n"
    putStrLn ("errors:\n" ++ (intercalate "\n" (map showError errors)))
    putStrLn "\n\n"


showCodeSection :: CodeSection -> String
showCodeSection sec =
    "CodeSection " ++ (sectionName sec) ++ " -----------------\n" ++
        intercalate "\n" (map showInstruction (instructions sec))


showError :: Error -> String
showError (Error       s) = "ERROR: " ++ s
showError (LineError l s) = "ERROR: " ++ s ++ "\n  on: " ++ showLine l


showSection :: Section -> String
showSection sec =
    "section " ++ (name sec) ++ "\n" ++
        intercalate "\n" (map showLine (contents sec)) ++ "\n"


showLine :: Line -> String
showLine l = do
    let lbls = labels l

    show (lineNumber l) ++ ":\n  " ++
        (if null lbls then "" else (intercalate "," lbls ++ ":\n  ")) ++ "|" ++
        intercalate "| |" (map showToken (tokens l)) ++ "|"


showToken :: Token -> String
showToken (Unquoted    s) = s
showToken (Quoted      s) = s
showToken (ControlChar c) = [c]


testFile =
    "; a comment\n" ++
    "section .data\n" ++
    --"   message:\n" ++
    --"   thing: db   \"here is a message \\\\ \\\" with quote\"\n" ++
    "# look more:\n" ++
    "section .text\n" ++
    "   _start:\n" ++
    "       mov rax, 123#a comment\n" ++
    --"       add qword rbx, [rcx + 2*rcx+rdx]\n" ++
    "       syscall"
