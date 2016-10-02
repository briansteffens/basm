module Main where

import Data.List
import Data.Binary.Put
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as B
import Debug.Trace (trace)

data Section = Section {
    kind :: T.Text,
    instructions :: [Instruction]
}

data Instruction = Instruction {
    labels :: [T.Text],             -- only for debug printing
    source :: T.Text,
    command :: T.Text,
    operands :: [Operand],
    size :: Int,
    offset :: Int
}

data Operand = Operand {
    text :: T.Text,
    operandSize :: Int,
    operandOffset :: Int
}

data Label = Label {
    name :: T.Text,
    instruction :: Instruction
}

data Relocation = Relocation {
    replace :: Operand,
    target :: Label
}

parseOperand :: T.Text -> [T.Text] -> Int -> Int -> Operand
parseOperand command opers index off = Operand (opers !! index) 8 off

parseOperands :: T.Text -> [T.Text] -> [T.Text] -> Int -> Int -> [Operand]
parseOperands _ _ [] _ _ = []
parseOperands command opers opersLeft index off = do
    let current = parseOperand command opers index off
    [current] ++ parseOperands command opers (tail opersLeft) (succ index)
                               (off + (operandSize current))

parseInstruction :: [T.Text] -> T.Text -> Int -> Instruction
parseInstruction labels source off = do
    let parts = T.break (== ' ') source
    let command = fst parts
    let opcodeSize = 2
    let operandText = [o | o <- map T.strip (T.split (== ',') (snd parts)),
                       not (T.null o)]
    let operands = parseOperands command operandText operandText 0
                                 (off + opcodeSize)

    let commandSize = opcodeSize + (sum [operandSize o | o <- operands])

    Instruction labels source (fst parts) operands commandSize off

parseInstructions :: [T.Text] -> Int -> ([Instruction], [Label])
parseInstructions [] _ = ([], [])
parseInstructions lines off = do
    -- Parse any previous-line labels
    let parts = break (\s -> (T.last s) /= ':') lines
    let preLabels = [T.init s | s <- fst parts]
    let inst = head (snd parts)

    -- Parse same-line label (message: db "hi")
    let labelParts = T.breakOn (T.pack ":") inst
    let noLabel = T.null (snd labelParts) ||
                  T.isInfixOf (T.pack "\"") (fst labelParts)
    let labelNames = preLabels ++ (if noLabel then [] else [fst labelParts])

    let source = if noLabel then inst
                 else (T.strip (T.drop 1 (snd labelParts)))

    let inst = parseInstruction labelNames source off

    let labels = [Label n inst | n <- labelNames]

    let inner = parseInstructions (drop 1 (snd parts)) (off + (size inst))
    ([inst] ++ (fst inner), labels ++ (snd inner))

parseSections :: T.Text -> [T.Text] -> ([Section], [Label])
parseSections _ [] = ([], [])
parseSections kind lines = do
    let startsWithSection = \s -> (T.take 9 s) == (T.pack "section .")
    let broken = break (startsWithSection) lines
    let remaining = snd broken
    let nextKind = trace (T.unpack kind) (T.drop 9 (head remaining))
    let parsed = parseInstructions (fst broken) 0
    let instructions = fst parsed
    let labels = snd parsed
    let section = Section kind instructions
    let inner = parseSections nextKind (drop 1 remaining)
    ([section] ++ (fst inner), labels ++ (snd inner))

getRelocation :: [Label] -> Operand -> [Relocation]
getRelocation labels operand = do
    let label = find (\l -> (name l) == (text operand)) labels
    case label of
        Just l -> [Relocation operand l]
        Nothing -> []

getRelocations :: [Section] -> [Label] -> [Relocation]
getRelocations sections labels = do
    let allInstructions = concat [instructions s | s <- sections]
    let allOperands = concat [operands i | i <- allInstructions]
    concat (map (getRelocation labels) allOperands)

trimLines x = map T.strip x
removeBlankLines x = [y | y <- x, not (T.null y)]
removeComments x = [y | y <- x, (T.head y) /= ';']

data QuoteChar = QuoteChar {
    char :: Char,
    quoted :: Bool,
    bracketed :: Bool
}

data TokenType = Quote | Bracket | Loose

data Token = Token {
    tokenType :: TokenType,
    contents :: [Char]
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

toText :: [QuoteChar] -> T.Text
toText x = (T.pack [char s | s <- x])

toTextArray :: [[QuoteChar]] -> [T.Text]
toTextArray x = [toText s | s <- x]

processLine :: [Char] -> [T.Text]
processLine = toTextArray .
              removeEmpty .
              splitQuoteChars .
              stripComment .
              parseQuotes

removeEmptyLines :: [[T.Text]] -> [[T.Text]]
removeEmptyLines input = [i | i <- input, (length i) > 0]

showLine lines = "LINE:\n  " ++ (intercalate "\n  " (map T.unpack lines))

main :: IO ()
main = do
    contents <- getContents
    let sourceLines = lines contents
    let processed = map processLine sourceLines
    let emptied = removeEmptyLines processed
    putStr (intercalate "\n" (map showLine emptied))

    --let (sections, labels) = parseSections (T.pack "none") emptied

    --let quoteChars = map parseQuotes sourceLines
    --putStr (intercalate "\n" (map showQuoteChars quoteChars))
    --let stripped = map stripComment quoteChars
    --putStr (intercalate "\n" (map showQuoteChars stripped))
    --let chunks = map splitQuoteChars stripped
    --putStr (intercalate "\n----\n" (map showQuoteChars chunks))
    --let emptied = removeEmpty chunks
    --putStr (intercalate "\n----\n" (map showQuoteChars emptied))
    --putStr "\nTEXTS:\n"
    --let texts = toTextArray emptied
    --putStr (intercalate "\n" (map T.unpack texts))
    putStr "\n"
    --contents <- getContents

    --let lines = ((removeComments .
    --              removeBlankLines .
    --              trimLines .
    --              T.lines .
    --              T.pack
    --             ) contents)

    --let (sections, labels) = parseSections (T.pack "none") lines
    --let relocations = getRelocations sections labels

    --putStr (intercalate "\n" (map showSection sections))

    --putStr "Relocations:\n  "
    --putStr (intercalate "\n  " (map showRelocation relocations))
    --putStr "\n"


showSection :: Section -> [Char]
showSection s = "[" ++ (T.unpack (kind s)) ++ "]\n" ++
                (intercalate "\n" (map showInstruction (instructions s)))

showInstruction :: Instruction -> [Char]
showInstruction i =
    (T.unpack (T.intercalate (T.pack ",") (labels i))) ++
    (if (null (labels i)) then "" else ":\n") ++
    ("  " ++ (show (offset i)) ++ ":\t" ++ (T.unpack (command i))) ++ "\n  " ++
    (intercalate "\n  " (map showOperand (operands i))) ++ "\n"

showOperand :: Operand -> [Char]
showOperand o = (show (operandOffset o)) ++ ":\t" ++ (T.unpack (text o))

showRelocation :: Relocation -> [Char]
showRelocation relocation =
    (show (operandOffset (replace relocation))) ++ " => " ++
    (T.unpack (name (target relocation)))
