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

main :: IO ()
main = do
    contents <- getContents

    let lines = ((removeComments .
                  removeBlankLines .
                  trimLines .
                  T.lines .
                  T.pack
                 ) contents)

    let (sections, labels) = parseSections (T.pack "none") lines
    let relocations = getRelocations sections labels

    putStr (intercalate "\n" (map showSection sections))

    putStr "Relocations:\n  "
    putStr (intercalate "\n  " (map showRelocation relocations))
    putStr "\n"


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
