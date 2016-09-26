module Main where

import qualified Data.Text as T

data Section = Section {
    kind :: T.Text,
    contents :: [T.Text]
}

trimLines x = map T.strip x
removeBlankLines x = [y | y <- x, not (T.null y)]
removeComments x = [y | y <- x, (T.head) y /= '#']

parseSections kind lines = do
    let startsWithSection = \s -> (T.take 9 s) == (T.pack "section .")
    let broken = break (startsWithSection) lines
    let remaining = snd broken
    let nextKind = T.drop 9 (head remaining)
    [Section kind (fst broken)] ++ parseSections nextKind (drop 1 remaining)

showSection :: Section -> IO ()
showSection s = do
    putStrLn ("[section " ++ (T.unpack (kind s)) ++ "]")
    putStr ((T.unpack . T.unlines) (contents s))

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
