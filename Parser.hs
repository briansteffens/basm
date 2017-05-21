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
import qualified Lexer as L


data Error =
      Error     String
    | LineError L.Line String
    deriving Show


data Section = Section {
    name     :: String,
    contents :: [L.Line]
} deriving Show


isLabel :: L.Token -> Bool
isLabel (L.Label _) = True
isLabel          _  = False


-- Check if a token is a symbol of the given type
matchSymbol :: L.SymbolType -> L.Token -> Bool
matchSymbol a (L.Symbol b) = a == b
matchSymbol _           _  = False


-- Take lines that consist of only labels and prepend those labels to the next
-- line
combineLabels :: [L.Line] -> [L.Token] -> [L.Line]
combineLabels [] [] = []
combineLabels [] _  = error("Labels at the end of file")
combineLabels (c:rem) carryOver
    | allLabels = recur $ carryOver ++ tokens
    | otherwise = [combined] ++ recur []
    where tokens    = L.tokens c
          allLabels = all isLabel $ tokens
          combined  = c { L.tokens = carryOver ++ tokens }
          recur     = combineLabels rem


-- Parse a directive from a list of tokens if possible
parseDirective :: [L.Token] -> Maybe Directive
parseDirective [L.Word "global", L.Word n] = Just $ Global STT_NOTYPE n
parseDirective [L.Word "global", L.Label n, L.Word t] =
    Just $ Global (read t) n
parseDirective [L.Word "extern", L.Word n] = Just $ Extern n
parseDirective _ = Nothing


-- Parse and extract directives from lines
extractDirectives :: [L.Line] -> ([Directive], [L.Line])
extractDirectives [] = ([], [])
extractDirectives (c:rem) = case parseDirective $ L.tokens c of
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
disp :: [L.Token] -> (Displacement, [L.Token])
disp [L.Number n]                      = (parseDisplacement  n       , [] )
disp (L.Number n:L.Symbol L.Plus :rem) = (parseDisplacement  n       , rem)
disp (L.Number n:L.Symbol L.Minus:rem) = (parseDisplacement  (-1 * n), rem)
disp [L.Word s]                        = (DisplacementSymbol DWORD s , [] )
disp (L.Word s  :L.Symbol L.Plus :rem) = (DisplacementSymbol DWORD s , rem)


-- Extract a scale and index from an operand if they exist
-- Expects tokens in reverse order
scale :: [L.Token] -> (Scale, Registers, [L.Token])
scale (L.Register r:L.Symbol L.Asterisk:L.Number n:rem)
    | isJust s  = (fromJust s, r, rem)
    | otherwise = (NoScale, NoRegister, rem)
    where s = parseScale n
scale t = (NoScale, NoRegister, t)


-- Extract a base register from an operand if one exists
-- Expects tokens in reverse order
base :: [L.Token] -> (Registers, [L.Token])
base (L.Symbol L.Plus:L.Register r:rem) = (r, rem)
base [L.Register r] = (r, [])
base t = (NoRegister, t)


-- Parse an indirect address operand like [rax+8*rbx+256]
indirect :: [L.Token] -> Maybe Operand
indirect t
    | null rem  = Just $ Address b s i d
    | otherwise = Nothing
    where (d,    t2 ) = disp $ reverse t
          (s, i, t3 ) = scale t2
          (b,    rem) = base t3


-- Parse an operand from a list of tokens
operand :: Command -> [L.Token] -> Maybe Operand
operand _   [L.Register r] = Just $ Register r
operand cmd [L.Word     w]
    | elem cmd jumpCommands = Just $ Relative  $ Symbol DWORD w
    | otherwise             = Just $ Immediate $ Symbol QWORD w
operand _   [L.Number   n]  = Just $ Immediate $ Literal literal
    where stripped = stripRight (== 0) (toBytes n)
          literal  = if null stripped then [0x00] else stripped
operand _ p = if not isIndirect then Nothing else indirect $ tail $ init p
    where isIndirect = length p > 2 &&
                       matchSymbol L.LeftBracket  (head p) &&
                       matchSymbol L.RightBracket (last p)


-- Parse a DB pseudo-instruction operand, which converts everything to bytes
dbOperand :: L.Token -> [Word8]
dbOperand (L.Number n) = [fromIntegral n :: Word8]
dbOperand (L.Quote  s) = map fromIntegral $ map ord s


-- Split a list of tokens on commas
splitOperands :: [L.Token] -> [[L.Token]]
splitOperands = split (matchSymbol L.Comma)


parseOperands :: Command -> [L.Token] -> Maybe [Operand]
parseOperands DB  t = Just [Immediate $ Literal $ concat $ map dbOperand t]
parseOperands cmd t = if failed then Nothing else Just opers
    where maybes = map (operand cmd) $ splitOperands t
          failed = any isNothing maybes
          opers  = map fromJust maybes


-- Remove the first token and parse it as a command mnemonic if possible
extractCommand :: [L.Token] -> Maybe (Command, [L.Token])
extractCommand (L.Command cmd:rem) = Just (cmd, rem)
extractCommand _ = Nothing


-- Make sure any size hints match and return any remaining tokens
extractSize :: [L.Token] -> Size -> [L.Token] -> Maybe (Size, [L.Token])
extractSize [] size nonSize = Just (size, nonSize)
extractSize (L.Word c:rem) prev nonSize
    | sizeMatch = extractSize rem newSize newNonSize
    | otherwise = Nothing
    where size           = readSize c
          haveSize       = size /= NoSize
          havePrev       = prev /= NoSize
          sizeMatch      = not havePrev || not haveSize || prev == size
          newSize        = if havePrev then prev else size
          newNonSize     = nonSize ++ if haveSize then [] else [L.Word c]
extractSize (c:rem) size nonSize = extractSize rem size (nonSize ++ [c])


-- Parse an instruction if possible
instruction :: L.Line -> Maybe Instruction
instruction line = do
    let (labels, rem1) =  break (\t -> not $ isLabel t) $ L.tokens line
    (cmd       , rem2) <- extractCommand rem1
    (size      , rem3) <- extractSize rem2 NoSize []
    opers              <- parseOperands cmd rem3

    Just $ Instruction {
        source     = L.sourceCode line,
        labelNames = map labelName labels,
        sizeHint   = size,
        command    = cmd,
        operands   = opers
    }

    where labelName (L.Label n) = n


-- Parse the name out of a section line if it is one
parseSectionName :: L.Line -> Maybe String
parseSectionName L.Line { L.tokens=[L.Word "section", L.Word n] } = Just n
parseSectionName _ = Nothing


-- Divide lines into sections
readSections :: [L.Line] -> [Section]
readSections [] = []
readSections (cur:rem) = [section] ++ recur
    where sectionName   = case parseSectionName cur of
                              Just n  -> n
                              Nothing -> error("Code found outside a section")
          (lines, next) = break (\l -> isJust $ parseSectionName l) rem
          section       = Section { name = sectionName, contents = lines }
          recur         = readSections next


-- Parse a section
codeSection :: Section -> Either [Error] CodeSection
codeSection sec
    | not $ null err = Left err
    | otherwise      = Right $ CodeSection {
        sectionName  = name sec,
        instructions = insts
    }
    where (err, insts) = partitionEithers $ map parseInst $ contents sec
          parseInst l  = case instruction l of
                             Just i  -> Right i
                             Nothing -> Left $ LineError l "Parser error"


-- Parse a list of lexer lines
parse :: [L.Line] -> Either [Error] ([Directive], [CodeSection])
parse lines
    | null err  = Right (dir, ret)
    | otherwise = Left $ concat err
    where (dir, rem) = extractDirectives $ combineLabels lines []
          (err, ret) = partitionEithers $ map codeSection $ readSections rem
