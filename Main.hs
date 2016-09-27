module Main where

import Data.List
import Data.Binary.Put
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

data Section = Section {
    kind :: T.Text,
    instructions :: [Instruction]
}

data Instruction = Instruction {
    labels :: [T.Text],
    source :: T.Text,
    command :: T.Text,
    operands :: [T.Text]
}

assembledLength :: Instruction -> Int
assembledLength inst = 8

parseInstruction :: [T.Text] -> T.Text -> Instruction
parseInstruction labels source = do
    let parts = T.break (== ' ') source
    let command = fst parts
    let operands = [o | o <- map T.strip (T.split (== ',') (snd parts)),
                    not (T.null o)]

    Instruction labels source command operands

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
    [Section kind (parseInstructions (fst broken))] ++
        parseSections nextKind (drop 1 remaining)

showSection :: Section -> IO ()
showSection s = do
    putStrLn ("[" ++ (T.unpack (kind s)) ++ "]")
    mapM_ showInstruction (instructions s)

showInstruction :: Instruction -> IO ()
showInstruction i = do
    putStr (T.unpack (T.intercalate (T.pack ",") (labels i)))
    putStr (if (null (labels i)) then "" else ">")
    putStr (T.unpack (command i))
    putStr (if (null (operands i)) then "" else "|")
    putStr (T.unpack (T.intercalate (T.pack ",") (operands i)))
    putStr "\n"

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

    --BL.putStr $ runPut assemble
