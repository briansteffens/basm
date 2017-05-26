module Parser where

import Data.Binary
import Data.Char
import Data.Either
import Data.Int
import Data.List
import Data.Maybe
import qualified Text.Read as TR

import Debug.Trace (trace)

import Shared
import Definitions
import Lexer


data Error =
      Error     String
    | LineError Line   String
    deriving Show


instance Display Error where
    display (Error       s) = "Error: " ++ s
    display (LineError l s) = "Error: " ++ s ++ "\n" ++ display l


isLabel :: Token -> Bool
isLabel (TLabel _) = True
isLabel         _  = False


-- Check if a token is a symbol of the given type
matchSymbol :: TSymbolType -> Token -> Bool
matchSymbol a (TSymbol b) = a == b
matchSymbol _          _  = False


-- Take lines that consist of only labels and prepend them to the next line
combineLabels :: [Line] -> [Token] -> [Line]
combineLabels [] [] = []
combineLabels [] _  = error("Labels at the end of file")
combineLabels (c:rem) carryOver
    | allLabels = recur $ carryOver ++ toks
    | otherwise = [combined] ++ recur []
    where toks      = tokens c
          allLabels = all isLabel $ toks
          combined  = c { tokens = carryOver ++ toks }
          recur     = combineLabels rem


-- Parse a directive from a list of tokens if possible
parseDirective :: [Token] -> Maybe Directive
parseDirective [TWord "global", TWord n] = Just $ Global STT_NOTYPE n
parseDirective [TWord "global", TLabel n, TWord t] = Just $ Global (read t) n
parseDirective [TWord "extern", TWord n] = Just $ Extern n
parseDirective _ = Nothing


-- Parse and extract directives from lines
extractDirectives :: [Line] -> ([Directive], [Line])
extractDirectives [] = ([], [])
extractDirectives (c:rem) = case parseDirective $ tokens c of
    Just d  -> concatTuple ([d], []) recur
    Nothing -> concatTuple ([], [c]) recur
    where recur           = extractDirectives rem


parseScale :: Integer -> Maybe Scale
parseScale 1 = Just NoScale
parseScale 2 = Just Scale2
parseScale 4 = Just Scale4
parseScale 8 = Just Scale8
parseScale _ = Nothing


parseDisplacement :: Integer -> Displacement
parseDisplacement i
    | i <= fromIntegral (maxBound :: Int8)  = Displacement8  (fromIntegral i)
    | i <= fromIntegral (maxBound :: Int32) = Displacement32 (fromIntegral i)
    | otherwise   = error("Invalid displacement value")


-- Extract a displacement from an operand if one exists
-- Expects tokens in reverse order
disp :: [Token] -> (Displacement, [Token])
disp [TNumber n]                   = (parseDisplacement  n       , [] )
disp (TNumber n:TSymbol Plus :rem) = (parseDisplacement  n       , rem)
disp (TNumber n:TSymbol Minus:rem) = (parseDisplacement  (-1 * n), rem)
disp [TWord s]                     = (DisplacementSymbol DWORD s , [] )
disp (TWord s  :TSymbol Plus :rem) = (DisplacementSymbol DWORD s , rem)
disp rem                           = (NoDisplacement             , rem)


-- Extract a scale and index from an operand if they exist
-- Expects tokens in reverse order
scale :: [Token] -> (Scale, Registers, [Token])
scale (TRegister r:TSymbol Asterisk:TNumber n:rem)
    | isJust s  = (fromJust s, r, rem)
    | otherwise = (NoScale, NoRegister, rem)
    where s = parseScale n
scale t = (NoScale, NoRegister, t)


-- Extract a base register from an operand if one exists
-- Expects tokens in reverse order
base :: [Token] -> (Registers, [Token])
base (TSymbol Plus:TRegister r:rem) = (r, rem)
base [TRegister r] = (r, [])
base t = (NoRegister, t)


-- Parse an indirect address operand like [rax+8*rbx+256]
indirect :: [Token] -> Maybe Operand
indirect t
    | null rem  = Just $ Address b s i d
    | otherwise = Nothing
    where (d,    t2 ) = disp $ reverse t
          (s, i, t3 ) = scale t2
          (b,    rem) = base t3


-- Parse an operand from a list of tokens
operand :: Command -> [Token] -> Maybe Operand
operand _   [TRegister r] = Just $ Register r
operand cmd [TWord     w]
    | elem cmd jumpCommands = Just $ Relative  $ Symbol DWORD w
    | otherwise             = Just $ Immediate $ Symbol QWORD w
operand _   [TNumber   n]  = Just $ Immediate $ Literal literal
    where stripped = stripRight (== 0) (toBytes n)
          literal  = if null stripped then [0x00] else stripped
operand _ p = if not isIndirect then Nothing else indirect $ tail $ init p
    where isIndirect = length p > 2 &&
                       matchSymbol LeftBracket  (head p) &&
                       matchSymbol RightBracket (last p)


-- Parse a DB pseudo-instruction operand, which converts everything to bytes
dbOperand :: Token -> [Word8]
dbOperand (TNumber n)     = [fromIntegral n :: Word8]
dbOperand (TQuote  s)     = map fromIntegral $ map ord s
dbOperand (TSymbol Comma) = []


-- Split a list of tokens on commas
splitOperands :: [Token] -> [[Token]]
splitOperands = split (matchSymbol Comma)


parseOperands :: Command -> [Token] -> Maybe [Operand]
parseOperands DB  t = Just [Immediate $ Literal $ concat $ map dbOperand t]
parseOperands cmd t = if failed then Nothing else Just opers
    where maybes = map (operand cmd) $ splitOperands t
          failed = any isNothing maybes
          opers  = map fromJust maybes


-- Remove the first token and parse it as a command mnemonic if possible
extractCommand :: [Token] -> Maybe (Command, [Token])
extractCommand (TCommand cmd:rem) = Just (cmd, rem)
extractCommand _ = Nothing


-- Make sure any size hints match and return any remaining tokens
extractSize :: [Token] -> Size -> [Token] -> Maybe (Size, [Token])
extractSize [] size nonSize = Just (size, nonSize)
extractSize (TWord c:rem) prev nonSize
    | sizeMatch = extractSize rem newSize newNonSize
    | otherwise = Nothing
    where size           = readSize c
          haveSize       = size /= NoSize
          havePrev       = prev /= NoSize
          sizeMatch      = not havePrev || not haveSize || prev == size
          newSize        = if havePrev then prev else size
          newNonSize     = nonSize ++ if haveSize then [] else [TWord c]
extractSize (c:rem) size nonSize = extractSize rem size (nonSize ++ [c])


-- Parse an instruction if possible
instruction :: Line -> Maybe Instruction
instruction line = do
    let (labels, rem1) =  break (\t -> not $ isLabel t) $ tokens line
    (cmd       , rem2) <- extractCommand rem1
    (size      , rem3) <- extractSize rem2 NoSize []
    opers              <- parseOperands cmd rem3

    Just $ Instruction {
        source     = sourceCode line,
        labelNames = map labelName labels,
        sizeHint   = size,
        command    = cmd,
        operands   = opers
    }

    where labelName (TLabel n) = n


-- Parse the name out of a section line if it is one
parseSectionName :: Line -> Maybe String
parseSectionName Line { tokens=[TWord "section", TWord n] } = Just n
parseSectionName _ = Nothing


-- Divide lines into sections
splitSections :: [Line] -> [(String, [Line])]
splitSections [] = []
splitSections (cur:rem) = [(sectionName, lines)] ++ recur
    where sectionName   = case parseSectionName cur of
                              Just n  -> n
                              Nothing -> error("Code found outside a section")
          (lines, next) = break (\l -> isJust $ parseSectionName l) rem
          recur         = splitSections next


-- Parse a section
section :: (String, [Line]) -> Either [Error] Section
section (name, lines)
    | not $ null err = Left err
    | otherwise      = Right $ Section {
        sectionName  = name,
        instructions = insts
    }
    where (err, insts) = partitionEithers $ map parseInst lines
          parseInst l  = case instruction l of
                             Just i  -> Right i
                             Nothing -> Left $ LineError l "Parser error"


-- Parse a list of lexer lines
parse :: [Line] -> Either [Error] ([Directive], [Section])
parse lines
    | null err  = Right (dir, ret)
    | otherwise = Left $ concat err
    where (dir, rem) = extractDirectives $ combineLabels lines []
          (err, ret) = partitionEithers $ map section $ splitSections rem
