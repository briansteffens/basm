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
    command :: T.Text,
    operands :: [Operand]
}

data Operand = Operand {
    text :: T.Text
}

data Label = Label {
    name :: T.Text,
    instruction :: Instruction
}

data Relocation = Relocation {
    replace :: Operand,
    target :: Label
}

--getRelocation :: [Label] -> Operand -> [Relocation]
--getRelocation labels operand = do
--    let label = find (\l -> (name l) == (text operand)) labels
--    case label of
--        Just l -> [Relocation operand l]
--        Nothing -> []
--
--getRelocations :: [Section] -> [Label] -> [Relocation]
--getRelocations sections labels = do
--    let allInstructions = concat [instructions s | s <- sections]
--    let allOperands = concat [operands i | i <- allInstructions]
--    concat (map (getRelocation labels) allOperands)

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

mergeLabelsInner :: [T.Text] -> [[T.Text]] -> [[T.Text]]
mergeLabelsInner _ [] = []
mergeLabelsInner labels remaining = do
    let current = head remaining

    let allLabels = all (\s -> (T.last s) == ':') current

    -- TODO: more expressive way to do these next two lines?
    let ret = case allLabels of False -> labels ++ current
                                True  -> []

    let innerLabels = case allLabels of False -> []
                                        True  -> labels ++ current

    let inner = mergeLabelsInner innerLabels (tail remaining)

    [ret] ++ inner

mergeLabels :: [[T.Text]] -> [[T.Text]]
mergeLabels input = mergeLabelsInner [] input

isLabel :: T.Text -> Bool
isLabel input = (T.last input) == ':'

-- Break same-line labels out into separate lines
breakOutLabels :: [[T.Text]] -> [[T.Text]]
breakOutLabels [] = []
breakOutLabels lines = do
    let current = head lines
    let remaining = tail lines

    let broken = break (\s -> not (isLabel s)) current

    let labels = [[l] | l <- (fst broken)]

    labels ++ [(snd broken)] ++ (breakOutLabels remaining)

parseInstructions :: [[T.Text]] -> ([Label], [Instruction])
parseInstructions [] = ([], [])
parseInstructions lines = do
    let current = head lines
    let parts = break (\s -> (not (isLabel s))) current
    let labelNames = fst parts
    let nonLabel = snd parts

    let operands = [Operand o | o <- tail nonLabel]
    let instruction = Instruction (head nonLabel) operands

    let labels = [Label n instruction | n <- labelNames]

    let inner = parseInstructions (tail lines)
    (labels ++ (fst inner), [instruction] ++ (snd inner))

main :: IO ()
main = do
    contents <- getContents
    let sourceLines = lines contents
    let processed = map processLine sourceLines
    let merged = mergeLabels processed
    let emptied = removeEmptyLines merged
    putStr (intercalate "\n" (map showLine emptied))

    let (labels, instructions) = parseInstructions emptied

    putStr (intercalate "\n" (map showInstruction instructions))

    putStr "\n"

    --let relocations = getRelocations sections labels


showSection :: Section -> [Char]
showSection s = "[" ++ (T.unpack (kind s)) ++ "]\n" ++
                (intercalate "\n" (map showInstruction (instructions s)))

showInstruction :: Instruction -> [Char]
showInstruction i =
    --(T.unpack (T.intercalate (T.pack ",") (labels i))) ++
    --(if (null (labels i)) then "" else ":\n") ++
    ((T.unpack (command i))) ++ "\n  " ++
    (intercalate "\n  " (map showOperand (operands i))) ++ "\n"

showOperand :: Operand -> [Char]
showOperand o = T.unpack (text o)

--showRelocation :: Relocation -> [Char]
--showRelocation relocation =
--    (show (operandOffset (replace relocation))) ++ " => " ++
--    (T.unpack (name (target relocation)))
