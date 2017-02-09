module Tokenizer where

import Data.List
import Data.Maybe


-- This file converts source code into tokens, grouped by line. Tokenization is
-- intended to occur between preprocessing and parsing.
--
-- The primary public interface to this functionality is the tokenize function.


-- Each token is a string, which can be either quoted or unquoted.
data Token = Unquoted    String -- The default: a mnemonic, operand, etc.
           | Quoted      String -- A quoted string literal
           | ControlChar Char   -- Comma, plus, etc.
           deriving Eq


-- Get the string value from a token.
tokenString :: Token -> String
tokenString (Unquoted    s) = s
tokenString (Quoted      s) = s
tokenString (ControlChar c) = [c]


-- This represents one tokenized line from the source code.
data Line = Line {
    code       :: String,   -- The source code of this line.
    lineNumber :: Int,      -- The line number within the file.
    labels     :: [String], -- The label names preceding this line.
    tokens     :: [Token]   -- The source code broken into tokens.
}


-- Characters that indicate the beginning of a comment.
isComment :: Char -> Bool
isComment c = elem c [';', '#']


-- Characters that delimit tokens but are also tokens themselves.
isControlChar :: Char -> Bool
isControlChar c = elem c [',', '[', ']', '+', '*']


-- Characters that delimit tokens.
isDelimiter :: Char -> Bool
isDelimiter c = elem c [' ', '\t', '\n'] || isControlChar c || isComment c


-- Convert a character from an escape sequence to its meaning (\n => ASCII 10).
resolveEscapeChar :: Char -> Char
resolveEscapeChar 'b'  = '\b'
resolveEscapeChar 'f'  = '\f'
resolveEscapeChar 'n'  = '\n'
resolveEscapeChar 'r'  = '\r'
resolveEscapeChar 't'  = '\t'
resolveEscapeChar '\\' = '\\'
resolveEscapeChar '"'  = '"'


-- Get the second character from a string if it has enough characters.
secondChar :: String -> Maybe Char
secondChar []       = Nothing
secondChar (_:[])   = Nothing
secondChar (_:x:xs) = Just x


-- Internal structure for stepTokenizer. An instance of this record describes
-- the state of the tokenizer while it's processing a single character.
data TokenizerState = TokenizerState {
    remaining :: String,        -- Unprocessed characters
    current   :: Char,          -- The character being processed
    previous  :: Maybe Char,    -- The character previously processed
    next      :: Maybe Char,    -- The next character to be processed
    buffer    :: String,        -- The unfinished token/label being built
    line      :: Line,          -- The current line being built
    inQuotes  :: Bool,
    isEscaped :: Bool,          -- The previous character was an escape slash
    inComment :: Bool           -- Comments last from ; or # to a newline
}


emptyLine = Line {
    code       = "",
    lineNumber = 1,
    labels     = [],
    tokens     = []
}


-- Convert a TokenizerState to a list of tokens, recursively, one character
-- at a time.
stepTokenizer :: TokenizerState -> [Line]
stepTokenizer state = do
    let cur = current state
    let curLine = line state

    let inQuote = inQuotes state

    let isEscapeSlash = inQuote && not (isEscaped state) && cur == '\\'

    let isQuote = cur == '"' && not isEscapeSlash && not (isEscaped state)

    let startOfQuote = isQuote && not inQuote
    let endOfQuote   = isQuote && inQuote

    -- Is the current character the end of a token?
    let delimiterIsNext n = not inQuote && isDelimiter n
    let isColon           = not inQuote && cur == ':'
    let endOfToken = case next state of Nothing -> True
                                        Just n  -> endOfQuote        ||
                                                   delimiterIsNext n ||
                                                   isColon

    -- Resolve escape character if necessary
    let resolved = if isEscaped state then resolveEscapeChar cur else cur

    let delimiter = not inQuote && isDelimiter cur

    -- Should the character be dropped (not saved into the token)?
    let skip = inComment state ||
               startOfQuote    ||
               endOfQuote      ||
               isEscapeSlash   ||
               delimiter

    -- Add character to token being built if necessary
    let newBuffer = (buffer state) ++ if skip then [] else [resolved]

    -- Complete new token if it's time
    let newToken = if endOfQuote then Quoted   newBuffer
                                 else Unquoted newBuffer

    -- Include comma/bracket tokens if needed
    let controlToken = if isControlChar cur then [ControlChar cur] else []

    let isNewThing = endOfToken && not (null newBuffer)
    let isNewToken = isNewThing && not isColon
    let isNewLabel = isNewThing && isColon

    -- Updated line
    let newLine = curLine {
        code   = (code curLine) ++ [cur],
        tokens = (tokens curLine) ++ if isNewToken then [newToken] else [] ++
                                     controlToken,
        labels = (labels curLine) ++ if isNewLabel then [init newBuffer]
                                                   else []
    }

    let newline = not inQuote && cur == '\n'
    let newInComment = not newline && (inComment state || isComment cur)
    let endOfLine = newline && not (null (tokens curLine))

    -- Recursive call
    let recur = stepTokenizer state {
        remaining = tail (remaining state),
        current   = head (remaining state),
        previous  = Just cur,
        next      = secondChar (remaining state),
        buffer    = if endOfToken then "" else newBuffer,
        inQuotes  = if inQuote then not endOfQuote else startOfQuote,
        isEscaped = isEscapeSlash,
        inComment = newInComment,
        line      = if not newline then newLine else emptyLine {
            lineNumber = succ (lineNumber (line state)),
            labels     = if not endOfLine then labels curLine else []
        }
    }

    let endOfInput = (next state) == Nothing

    let ret = if endOfLine || endOfInput then [newLine] else []
    if endOfInput then ret else ret ++ recur


-- Tokenize source code.
tokenize :: String -> [Line]
tokenize src =
    stepTokenizer TokenizerState {
        remaining = tail src,
        previous  = Nothing,
        current   = head src,
        next      = secondChar src,
        buffer    = "",
        inQuotes  = False,
        isEscaped = False,
        inComment = False,
        line      = emptyLine
    }
