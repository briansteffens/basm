module Parser where

import Data.List
import qualified Data.Map as M

import Preprocessor

data Section = Section {
    kind :: [Char],
    instructions :: [Instruction],
    sectionIndex :: Int
}

data Instruction = Instruction {
    labels :: [[Char]],
    command :: [Char],
    operands :: [Operand],
    instructionOffset :: Int
}

data Operand = Operand {
    text :: [Char],
    operandOffset :: Int,
    operandSize :: Int
}

-- Extract global/%define directives from the code
extractNonSectionLines :: [[[Char]]] -> ([[[Char]]], [[[Char]]])
extractNonSectionLines [] = ([], [])
extractNonSectionLines lines = do
    let current = head lines

    let isNonSection = elem (head current) ["global", "%define"]

    let retLine = case isNonSection of False -> [current]
                                       True  -> []

    let retGlobal = case isNonSection of False -> []
                                         True  -> [current]

    let inner = extractNonSectionLines (tail lines)
    (retLine ++ fst inner, retGlobal ++ snd inner)

extractGlobals :: [[[Char]]] -> [[Char]]
extractGlobals [] = []
extractGlobals lines = do
    let current = head lines

    if (head current) == "global" then [current !! 1] else [] ++
        extractGlobals (tail lines)

extractDefines :: [[[Char]]] -> [([Char], [Char])]
extractDefines [] = []
extractDefines lines = do
    let current = head lines
    let rest = tail current

    let ret = if (head current) /= "%define" then []
        else [(head rest, intercalate " " (tail rest))]

    ret ++ extractDefines (tail lines)

parseOperands :: [[Char]] -> Int -> [Operand]
parseOperands [] _ = []
parseOperands operands offset = do
    let ret = Operand (head operands) offset 0

    let inner = parseOperands (tail operands) (offset + (operandSize ret))

    [ret] ++ inner

parseInstructionsInner :: [[[Char]]] -> Int -> [Instruction]
parseInstructionsInner [] _ = []
parseInstructionsInner lines offset = do
    let current = head lines
    let parts = break (\s -> last s /= ':') current
    let labels = [init l | l <- fst parts]
    let nonLabel = snd parts
    let operandStrings = tail nonLabel
    let command = head nonLabel

    let size = commandSize command

    let operands = parseOperands operandStrings (offset + size)
    let instruction = Instruction labels command operands offset

    let operandsSize = sum [operandSize o | o <- operands]

    [instruction] ++ parseInstructionsInner (tail lines)
                                            (offset + size + operandsSize)

parseInstructions :: [[[Char]]] -> [Instruction]
parseInstructions lines = parseInstructionsInner lines 0

parseSectionsInner :: [[[Char]]] -> [Char] -> Int -> [Section]
parseSectionsInner [] _ _ = []
parseSectionsInner lines kind index = do
    let broken = break (\s -> head s == "section") lines

    let instructions = parseInstructions (fst broken)

    let ret = Section kind instructions index

    let nextSection = drop 1 ((head (snd broken)) !! 1)

    let anyLeft = not (null (snd broken))
    let remaining = case anyLeft of False -> []
                                    True  -> tail (snd broken)

    [ret] ++ parseSectionsInner remaining nextSection (succ index)

parseSections :: [[[Char]]] -> [Section]
parseSections lines = parseSectionsInner lines "base" 0

commandSize :: [Char] -> Int
commandSize cmd =
    case cmd of "syscall" -> 2
                "mov"     -> 2
                "db"      -> 0
                "jmp"     -> 1
                "je"      -> 2 -- TODO: support je short, not just je near.
                "cmp"     -> 3 -- TODO: needs to be 2 if weird case

applyDefines :: [Section] -> M.Map [Char] [Char] -> [Section]
applyDefines sections defines = do
    let processOperand oper = case M.lookup (text oper) defines of
                              Nothing -> oper
                              Just t  -> oper { text = t }

    let processInstruction inst = inst {
        operands = map processOperand (operands inst)
    }

    let processSection sec = sec {
        instructions = map processInstruction (instructions sec)
    }

    map processSection sections

-- Convert a string of code into a tuple of sections and globals
parse :: [Char] -> ([Section], [[Char]])
parse input = do
    let (parsedLines, nonSectionLines) = extractNonSectionLines .
                                         preprocess $ input

    let globals = extractGlobals nonSectionLines
    let defines = M.fromList (extractDefines nonSectionLines)

    let sections = applyDefines (tail . parseSections $ parsedLines) defines

    (sections, globals)
