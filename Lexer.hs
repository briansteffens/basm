module Main where

import Data.Char
import Data.List
import Data.Maybe
import Text.Show.Functions

import Debug.Trace (trace)

import Shared


data SymbolType = Comma
                | Dollar
                | LeftBracket
                | RightBracket
                | Plus
                | Minus
                | Asterisk
                deriving (Eq, Show)


data Token = Word   String
           | Label  String
           | Quote  String
           | Symbol SymbolType
           | Number Integer
           deriving (Eq, Show)


data Line = Line {
    sourceCode :: String,
    lineNumber :: Int,
    tokens     :: [Token]
}


readSymbolType :: Char -> Maybe SymbolType
readSymbolType ',' = Just Comma
readSymbolType '$' = Just Dollar
readSymbolType '[' = Just LeftBracket
readSymbolType ']' = Just RightBracket
readSymbolType '+' = Just Plus
readSymbolType '-' = Just Minus
readSymbolType '*' = Just Asterisk
readSymbolType  _  = Nothing


-- Read characters until a delimiter into a Word or Label
consumeWord :: String -> String -> ([Token], String)
consumeWord []       acc = ([Word  acc], [])
consumeWord (':':rem) acc = ([Label acc], rem)
consumeWord (c  :rem) acc
    | isDelimiter = ([Word acc], [c] ++ rem)
    | otherwise   = consumeWord rem (acc ++ [c])
    where isDelimiter = elem c [' ', '\t', '\n', ';', '#'] ||
                        isJust (readSymbolType c)


consumeNumber :: String -> String -> ([Token], String)
consumeNumber []      acc = ([Number $ read acc], [])
consumeNumber (c:rem) acc
    | isDigit c = consumeNumber rem (acc ++ [c])
    | otherwise = ([Number $ read acc], [c] ++ rem)


-- Read one character from a quote
consumeQuoteChar :: String -> (Maybe Char, String)
consumeQuoteChar []             = error("Unexpected end of quote")
consumeQuoteChar ('\\':'n' :rem) = (Just '\n', rem)
consumeQuoteChar ('\\':'t' :rem) = (Just '\t', rem)
consumeQuoteChar ('\\':'\\':rem) = (Just '\\', rem)
consumeQuoteChar ('\\':'\'':rem) = (Just '\'', rem)
consumeQuoteChar ('\\':'"' :rem) = (Just '"' , rem)
consumeQuoteChar ('"' :'"' :rem) = (Just '"' , rem)
consumeQuoteChar ('"'      :rem) = (Nothing  , rem)
consumeQuoteChar (c        :rem) = (Just c   , rem)


-- Read the contents of a string until the closing quote
consumeQuote :: String -> String -> ([Token], String)
consumeQuote acc rem = case consumeQuoteChar rem of
    (Just c , rem) -> consumeQuote (acc ++ [c]) rem
    (Nothing, rem) -> ([Quote acc], rem)


-- Read the next token from a string
lexToken :: String -> ([Token], String)
lexToken (' ' :rem) = ([], rem)
lexToken ('\t':rem) = ([], rem)
lexToken (';' :rem) = ([], [])
lexToken ('"' :rem) = consumeQuote "" rem
lexToken (c   :rem)
    | isDigit c = consumeNumber ([c] ++ rem) ""
    | otherwise = case readSymbolType c of
        Just s  -> ([Symbol s], rem)
        Nothing -> consumeWord ([c] ++ rem) ""


-- Read all tokens from a string
lexLine :: String -> [Token]
lexLine []  = []
lexLine str = tokens ++ lexLine remaining
    where (tokens, remaining) = lexToken str


-- Tokenize source code.
lexer' :: [String] -> Int -> [Line]
lexer' []     _   = []
lexer' (c:rem) num = [line] ++ lexer' rem (succ num)
    where line = Line {
        sourceCode = c,
        lineNumber = num,
        tokens = lexLine c
    }


-- Bootstrap the lexer
lexer :: String -> [Line]
lexer src = lexer' (split (== '\n') src) 1


-- debug stuff ----------------------------------------------------------------


instance Show Line where
    show line = num ++ " " ++ code ++ "\n" ++ toks ++ "\n"
        where num  = show $ lineNumber line
              code = sourceCode line
              toks = intercalate "\n" $ map ("- " ++) $
                     map show $ tokens line


main :: IO ()
main = do
    let source = "section .data\n  msg: db \"hi \"\" there\"\n" ++
                 "global _start\n; some comment\n_start:\n    mov rax, 60\n"
    let lines = lexer source
    --let toks = concat $ map tokens lines
    --putStrLn $ intercalate "\n" $ map show $ concat $ map tokens lines
    putStrLn $ intercalate "\n" $ map show lines
