module Main where

import Data.List
import Data.Maybe

import Definitions
import Tokenizer


data Section = Section {
    name     :: String,
    contents :: [Line]
}


data Error = Error     String
           | LineError Line   String


-- Check if a token is quoted.
isQuoted :: Token -> Bool
isQuoted (Unquoted _) = False
isQuoted (Quoted   _) = True


-- Parse a line as a section directive, returning the section name if it is
-- a section directive and Nothing if not.
parseSectionName :: Line -> (Maybe String, [Error])
parseSectionName line = do
    let firstParam = head (tokens line)
    let secondParam = last (tokens line)

    let isSection = firstParam == Unquoted "section"
    let correctParamCount = length (tokens line) == 2
    let isValid = correctParamCount && not (isQuoted secondParam)

    let errors = if not isSection then [] else if isValid
                     then []
                     else [LineError line "Invalid syntax"]

    let ret = if isSection && correctParamCount
                  then Just (tokenString secondParam)
                  else Nothing

    (ret, errors)


-- Break a list into 3 chunks based on a predicate.
doubleBreak :: (a -> Bool) -> [a] -> ([a], [a], [a])
doubleBreak predicate list = do
    let (before, remaining) = break predicate list
    let (current, after) = break predicate (tail remaining)

    if null remaining
        then (before, [], [])
        else (before, [head remaining] ++ current, after)


-- Divide a list of lines into sections.
divideSections :: [Line] -> ([Section], [Error])
divideSections []    = ([], [])
divideSections lines = do
    let isSection l = head (tokens l) == Unquoted "section"
    let (before, current, after) = doubleBreak isSection lines

    let sectionLine = head current

    let outsideSectionErrors = if null before then []
                               else [LineError sectionLine
                                               "Code found outside section"]

    let (sectionName, parseErrors) = parseSectionName sectionLine

    let section = Section {
        name     = fromJust sectionName,
        contents = tail current
    }

    let errors = outsideSectionErrors ++ parseErrors

    let (recurSections, recurErrors) = divideSections after
    ([section] ++ recurSections, errors ++ recurErrors)


-- Convert tokens into an AST.
parse :: [Line] -> [CodeSection]
parse [] = []


main :: IO ()
main = do
    let toks = tokenize testFile
    putStrLn ((intercalate "\n" (map showLine toks)) ++ "\n")

    let (secs, errors) = divideSections toks
    let out = if null errors
                then intercalate "\n" (map showSection secs)
                else intercalate "\n" (map showError errors)
    putStrLn out


showError :: Error -> String
showError (Error       s) = "ERROR: " ++ s
showError (LineError l s) = "ERROR: " ++ s ++ "\n  on: " ++ showLine l


showSection :: Section -> String
showSection sec =
    "section " ++ (name sec) ++ "\n" ++
        intercalate "\n" (map showLine (contents sec)) ++ "\n"


showLine :: Line -> String
showLine l = do
    let lbls = labels l

    show (lineNumber l) ++ " " ++
        (if null lbls then "" else (intercalate "," lbls ++ ": ")) ++ "|" ++
        intercalate "| |" (map showToken (tokens l)) ++ "|"


showToken :: Token -> String
showToken (Unquoted s) = s
showToken (Quoted   s) = s


testFile =
    "; a comment\n" ++
    "section .data\n" ++
    "   message:\n" ++
    "   thing: db   \"here is a message \\\\ \\\" with quote\"\n" ++
    "# look more:\n" ++
    "section .text\n" ++
    "   _start:\n" ++
    "       mov rax, 123#a comment\n" ++
    "       syscall"
