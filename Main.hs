module Main where

import Data.List
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as B
import Debug.Trace (trace)

data Section = Section {
    kind :: [Char],
    instructions :: [Instruction]
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

--data Relocation = Relocation {
--    replace :: Operand,
--    target :: Label
--}

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
commandSize cmd = 4

parseOperands :: [[Char]] -> Int -> [Operand]
parseOperands [] _ = []
parseOperands operands offset = do
    let ret = Operand (head operands) offset 8

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

parseSectionsInner :: [[[Char]]] -> [Char] -> [Section]
parseSectionsInner [] _ = []
parseSectionsInner lines kind = do
    let broken = break (\s -> head s == "section") lines

    let instructions = parseInstructions (fst broken)

    let ret = Section kind instructions

    let nextSection = drop 1 (head (head (snd broken)))

    let anyLeft = not (null (snd broken))
    let remaining = case anyLeft of False -> []
                                    True  -> tail (snd broken)

    [ret] ++ parseSectionsInner remaining nextSection

parseSections :: [[[Char]]] -> [Section]
parseSections lines = parseSectionsInner lines "base"

main :: IO ()
main = do
    contents <- getContents

    let sourceLines = lines contents
    let processed = map processLine sourceLines
    let merged = mergeLabels processed
    let emptied = removeEmptyLines merged
    putStr (intercalate "\n" (map showLine emptied))

    let sections = parseSections emptied
    putStr (showSections sections)

    putStr "\n"

    --let relocations = getRelocations sections labels

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

--showRelocation :: Relocation -> [Char]
--showRelocation relocation =
--    (show (operandOffset (replace relocation))) ++ " => " ++
--    (T.unpack (name (target relocation)))
