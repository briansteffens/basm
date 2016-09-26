module Main where

import Data.List
import qualified Data.Text as T

data Section = Section {
    kind :: T.Text,
    instructions :: [Instruction]
}

data Instruction = Instruction {
    labels :: [T.Text],
    source :: T.Text
}

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

    [Instruction labels source] ++ parseInstructions (drop 1 (snd parts))

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
    putStr (if (null (labels i)) then "" else "> ")
    putStrLn (T.unpack (source i))

trimLines x = map T.strip x
removeBlankLines x = [y | y <- x, not (T.null y)]
removeComments x = [y | y <- x, (T.head) y /= ';']

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
