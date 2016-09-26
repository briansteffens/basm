module Main where

import qualified Data.Text as T

data Section = Section {
    kind :: String,
    lines :: [String]
}

sum' [] = 0
sum' (x:xs) = x + sum' xs

trimLines x = map T.strip x
removeBlankLines x = [y | y <- x, not (T.null y)]
removeComments x = [y | y <- x, (T.head) y /= '#']

main :: IO ()
main = do
    contents <- getContents

    putStr ((T.unpack .
             T.unlines .
             removeComments .
             removeBlankLines .
             trimLines .
             T.lines .
             T.pack
            ) contents)
