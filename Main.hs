module Main where

import Data.List
import Data.Binary.Put
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as B

data Section = Section {
    kind :: T.Text,
    instructions :: [Instruction],
    tokens :: [Token]
}

data Instruction = Instruction {
    labels :: [T.Text],
    operands :: [T.Text]
}

data Relocation = Relocation {
    sourceSection :: Section,
    sourceOffset :: Int,
    targetSection :: Section,
    targetOffset :: Int
}

data Label = Label {
    label :: T.Text,
    target :: Token
}

data Token = Token {
    instruction :: Instruction,
    operandIndex :: Int,
    source :: T.Text,
    offset :: Int,
    size :: Int
}

getTokens :: Instruction -> Int -> Int -> [T.Text] -> [Token]
getTokens inst index offset [] = []
getTokens inst index offset operandsRemaining = do
    let size = 8
    [Token inst index (head operandsRemaining) offset size] ++
        (getTokens inst (succ index) (offset + size) (tail operandsRemaining))

instructionToTokens :: Instruction -> [Token]
instructionToTokens inst = do
    getTokens inst 0 0 (operands inst)

generateTokens :: Section -> (Section, [Label])
generateTokens section = do
    let insts = instructions section
    let tokens = concat [instructionToTokens i | i <- insts]
    (Section (kind section) insts tokens, [])

parseInstruction :: [T.Text] -> T.Text -> Instruction
parseInstruction labels source = do
    let parts = T.break (== ' ') source
    let operands = [fst parts] ++
                   [o | o <- map T.strip (T.split (== ',') (snd parts)),
                    not (T.null o)]

    Instruction labels operands

parseInstructions [] = []
parseInstructions lines = do
    -- Parse any previous-line labels
    let parts = break (\s -> (T.last s) /= ':') lines
    let preLabels = [T.init s | s <- fst parts]
    let inst = head (snd parts)

    -- Parse same-line label (message: db "hi")
    let labelParts = T.breakOn (T.pack ":") inst
    let noLabel = T.null (snd labelParts) ||
                  T.isInfixOf (T.pack "\"") (fst labelParts)
    let labels = preLabels ++ (if noLabel then [] else [fst labelParts])

    let source = if noLabel then inst
                 else (T.strip (T.drop 1 (snd labelParts)))

    [parseInstruction labels source] ++ parseInstructions (drop 1 (snd parts))

parseSections kind lines = do
    let startsWithSection = \s -> (T.take 9 s) == (T.pack "section .")
    let broken = break (startsWithSection) lines
    let remaining = snd broken
    let nextKind = T.drop 9 (head remaining)
    [Section kind (parseInstructions (fst broken)) []] ++
        parseSections nextKind (drop 1 remaining)

showSection :: Section -> IO ()
showSection s = do
    putStrLn ("[" ++ (T.unpack (kind s)) ++ "]")
    mapM_ showInstruction (instructions s)

showInstruction :: Instruction -> IO ()
showInstruction i = do
    putStr (T.unpack (T.intercalate (T.pack ",") (labels i)))
    putStr (if (null (labels i)) then "" else ":")
    putStr (T.unpack (T.intercalate (T.pack "|") (operands i)))
    putStr "\n"

showRelocation :: Relocation -> IO ()
showRelocation r = do
    putStr "Relo "
    putStr (T.unpack (kind (sourceSection r)))
    putStr "+"
    putStr (show (sourceOffset r))
    putStr " -> "
    putStr (T.unpack (kind (targetSection r)))
    putStr "+"
    putStr (show (targetOffset r))
    putStr "\n"

showToken :: Token -> [Char]
showToken t = (show (offset t)) ++ ": " ++ (T.unpack (source t))

showTokens :: [Token] -> [Char]
showTokens t = intercalate "\n" (map showToken t)

showSectionTokens :: Section -> [Char]
showSectionTokens s = "[" ++ (T.unpack (kind s)) ++ "]\n" ++
                      (showTokens (tokens s))

trimLines x = map T.strip x
removeBlankLines x = [y | y <- x, not (T.null y)]
removeComments x = [y | y <- x, (T.head) y /= ';']

assemble :: Put
assemble = do
  putWord32be 1
  putWord16be 2
  putWord8 3

main :: IO ()
main = do
    contents <- getContents

    let lines = ((removeComments .
                  removeBlankLines .
                  trimLines .
                  T.lines .
                  T.pack
                 ) contents)

    let sections = parseSections (T.pack "none") lines

    putStr "\n"
    showSection (sections !! 0)
    putStr "\n"
    showSection (sections !! 1)
    putStr "\n"
    showSection (sections !! 2)
    putStr "\n"

    putStr "--------- TOKENS ------------\n"
    let sections2 = [fst a | a <- (map generateTokens sections)]

    putStr (intercalate "\n\n" (map showSectionTokens sections2))

    --let x = runPut assemble
    --BL.putStr x
    --BL.putStr $ runPut assemble
