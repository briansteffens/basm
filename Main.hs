module Main where

import System.Environment
import Data.List
import qualified Data.ByteString.Lazy as B

import qualified Parser as P
import qualified Elf as E


main :: IO ()
main = do
    args <- getArgs

    let filename = if length args == 1 then head args
                                       else error("Usage: basm <filename>")

    contents <- readFile filename

    let (sections, directives, errors, parseDebug) = P.parse contents

    putStrLn "basm lexer ---------------------------------------------------\n"
    putStrLn parseDebug
    putStrLn "\nbasm ast -----------------------------------------------------"
    putStrLn ("\n" ++ (intercalate "\n\n" (map P.showCodeSection sections)))
    putStrLn "\nbasm results -------------------------------------------------"
    putStrLn ("\nerrors:\n" ++ (intercalate "\n" (map P.showError errors)))

    let (bytes, debug) = E.assemble sections directives

    --putStrLn debug

    let objFilename = (takeWhile (/= '.') filename) ++ ".o"
    B.writeFile objFilename bytes
