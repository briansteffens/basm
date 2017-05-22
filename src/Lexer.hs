module Lexer where

import Data.Char
import Data.List
import Data.Maybe
import Text.Show.Functions
import qualified Text.Read as TR

import Debug.Trace (trace)

import Shared
import Definitions


data TSymbolType
    = Comma
    | Dollar
    | LeftBracket
    | RightBracket
    | Plus
    | Minus
    | Asterisk
    deriving (Eq, Show)


data Token
    = TWord     String
    | TCommand  Command
    | TRegister Registers
    | TLabel    String
    | TQuote    String
    | TSymbol   TSymbolType
    | TNumber   Integer
    deriving (Eq, Show)


data Line = Line {
    sourceCode :: String,
    lineNumber :: Int,
    tokens     :: [Token]
}   deriving (Eq, Show)


instance Display Line where
    display line = "[" ++ show (lineNumber line) ++ "] " ++ (sourceCode line)


readSymbolType :: Char -> Maybe TSymbolType
readSymbolType ',' = Just Comma
readSymbolType '$' = Just Dollar
readSymbolType '[' = Just LeftBracket
readSymbolType ']' = Just RightBracket
readSymbolType '+' = Just Plus
readSymbolType '-' = Just Minus
readSymbolType '*' = Just Asterisk
readSymbolType  _  = Nothing


-- Read a string into either a TRegister, TCommand, or TWord
readString :: String -> Token
readString s
    | isJust register = TRegister $ fromJust register
    | isJust command  = TCommand  $ fromJust command
    | otherwise       = TWord s
    where upper    = map toUpper s
          register = TR.readMaybe upper :: Maybe Registers
          command  = TR.readMaybe upper :: Maybe Command


-- Read characters until a delimiter into a TWord or TLabel
consumeWord :: String -> String -> ([Token], String)
consumeWord []        acc = ([readString acc], [])
consumeWord (':':rem) acc = ([TLabel     acc], rem)
consumeWord (c  :rem) acc
    | isDelimiter = ([readString acc], [c] ++ rem)
    | otherwise   = consumeWord rem (acc ++ [c])
    where isDelimiter = elem c [' ', '\t', '\n', ';', '#'] ||
                        isJust (readSymbolType c)


-- Read digits until a non-digit into a Number
consumeNumber :: String -> String -> ([Token], String)
consumeNumber []      acc = ([TNumber $ read acc], [])
consumeNumber (c:rem) acc
    | isDigit c = consumeNumber rem (acc ++ [c])
    | otherwise = ([TNumber $ read acc], [c] ++ rem)


-- Read one character from a quote
consumeQuoteChar :: String -> (Maybe Char, String)
consumeQuoteChar []              = error("Unexpected end of quote")
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
    (Nothing, rem) -> ([TQuote acc], rem)


-- Read the next token from a string
lexToken :: String -> ([Token], String)
lexToken (' ' :rem) = ([], rem)
lexToken ('\t':rem) = ([], rem)
lexToken (';' :rem) = ([], [])
lexToken ('"' :rem) = consumeQuote "" rem
lexToken (c   :rem)
    | isDigit c = consumeNumber ([c] ++ rem) ""
    | otherwise = case readSymbolType c of
        Just s  -> ([TSymbol s], rem)
        Nothing -> consumeWord ([c] ++ rem) ""


-- Read all tokens from a string
lexLine :: String -> [Token]
lexLine []  = []
lexLine str = tokens ++ lexLine remaining
    where (tokens, remaining) = lexToken str


-- Tokenize source code.
lexer' :: [String] -> Int -> [Line]
lexer' []      _   = []
lexer' (c:rem) num = [line] ++ lexer' rem (succ num)
    where line = Line {
        sourceCode = c,
        lineNumber = num,
        tokens = lexLine c
    }


-- Bootstrap the lexer
lexer :: String -> [Line]
lexer src = lexer' (split (== '\n') src) 1
