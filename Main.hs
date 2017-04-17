module Main where

import System.Environment
import Data.List
import Control.Monad
import qualified Data.ByteString.Lazy as B

import qualified Parser as P
import qualified Elf as E


allAfterFirstDot :: String -> String
allAfterFirstDot ('.':xs) = xs
allAfterFirstDot ( _ :xs) = allAfterFirstDot xs


chopExtension :: String -> String
chopExtension fn = reverse (allAfterFirstDot (reverse fn))


data Args = Args {
    filename :: String,
    verbose  :: Bool
}


parseArg :: String -> Args -> Args
parseArg "-v" args = args { verbose  = True }
parseArg fn   args = args { filename = fn }


parseArgs' :: [String] -> Args -> Args
parseArgs' []     args = args
parseArgs' (a:as) args = parseArgs' as (parseArg a args)


parseArgs :: [String] -> Args
parseArgs as = parseArgs' as Args {
    filename = [],
    verbose = False
}


main :: IO ()
main = do
    argList <- getArgs

    let args = if null argList then error("Usage: basm <filename>")
                               else parseArgs argList

    contents <- readFile (filename args)

    let (sections, directives, errors, parseDebug) = P.parse contents

    let o = intercalate "\n" [
           "basm lexer ---------------------------------------------------\n",
           parseDebug,
           "\nbasm ast -----------------------------------------------------",
           ("\n" ++ (intercalate "\n\n" (map P.showCodeSection sections))),
           "\nbasm results -------------------------------------------------",
           ("\nerrors:\n" ++ (intercalate "\n" (map P.showError errors)))]

    when (verbose args) $ putStrLn o

    let (bytes, debug) = E.assemble sections directives

    --putStrLn debug

    let objFilename = (chopExtension (filename args)) ++ ".o"
    B.writeFile objFilename bytes
