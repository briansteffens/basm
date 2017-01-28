module Main where

import Data.List
import Data.Maybe


data Token = Unquoted String
           | Quoted   String


data Line = Line {
    source     :: String,
    lineNumber :: Int,
    tokens     :: [Token]
}


data TokenizerState = TokenizerState {
    remaining :: String,        -- Unprocessed characters
    current   :: Char,          -- The character being processed
    previous  :: Maybe Char,    -- The character previously processed
    next      :: Maybe Char,    -- The next character to be processed
    building  :: String,        -- The token being built
    inQuotes  :: Bool,
    built     :: [Token],       -- The tokens built so far
    isEscaped :: Bool           -- The previous character was an escape slash
}


-- Characters the indicate the beginning of a comment.
isComment :: Char -> Bool
isComment c = elem c [';', '#']


-- Characters that delimit tokens.
isDelimiter :: Char -> Bool
isDelimiter c = elem c [' ', '\t', '\n', ','] || isComment c


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
secondChar []     = Nothing
secondChar [_]    = Nothing
secondChar (_:xs) = Just (head xs)


-- Convert a TokenizerState to a list of tokens, recursively, one character
-- at a time.
stepTokenizer :: TokenizerState -> [Token]
stepTokenizer state = do
    let cur = current state

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
    let skip = startOfQuote  ||
               endOfQuote    ||
               isEscapeSlash ||
               delimiter

    -- Add character to token being built if necessary
    let newBuilding = (building state) ++ if skip then [] else [resolved]

    -- Complete new token if it's time
    let newToken = if endOfQuote then Quoted   newBuilding
                                 else Unquoted newBuilding

    let newTokens = built state ++ if endOfToken && not (null newBuilding)
                                   then [newToken] else []

    -- Recursive call
    let recur = stepTokenizer state {
        remaining = tail (remaining state),
        current   = head (remaining state),
        previous  = Just cur,
        next      = secondChar (remaining state),
        building  = if endOfToken then "" else newBuilding,
        built     = newTokens,
        inQuotes  = if inQuote then not endOfQuote else startOfQuote,
        isEscaped = isEscapeSlash
    }

    let endOfInput = (next state) == Nothing || isComment cur

    -- Return token list if there's nothing left to process, otherwise recur
    if endOfInput then newTokens else recur


-- Convert a line of source code into tokens if there are any tokens.
tokenizeLine :: (String, Int) -> Maybe Line
tokenizeLine ([],  _  ) = Nothing
tokenizeLine (src, num) = do
    let toks = stepTokenizer TokenizerState {
        remaining = tail src,
        previous  = Nothing,
        current   = head src,
        next      = secondChar src,
        building  = "",
        built     = [],
        inQuotes  = False,
        isEscaped = False
    }

    let line = Line {
        source     = src,
        lineNumber = num,
        tokens     = toks
    }

    if null toks then Nothing else Just line
     

-- Tokenize some source code.
tokenize :: String -> [Line]
tokenize src =
    concat (map maybeToList (map tokenizeLine (zip (lines src) [1..])))


main :: IO ()
main = do
    putStrLn (intercalate "\n" (map showLine (tokenize testFile)))


showLine :: Line -> String
showLine l = show (lineNumber l) ++ ": |" ++
             intercalate "| |" (map showToken (tokens l)) ++ "|"


showToken :: Token -> String
showToken (Unquoted s) = s
showToken (Quoted   s) = s


testFile =
    "%define some(a) thing a \\\n" ++
    "        more stuff here\n" ++
    "\n" ++
    "; a comment\n" ++
    "      ; another comment\n" ++
    "section .data\n" ++
    "       here: \"is a message \\\" yes\" \n" ++
    "section .text\n" ++
    "    mov rax, 123;A COMMENT\n" ++
    "           \t\tmov rbx, 321 ; comment!\n"
