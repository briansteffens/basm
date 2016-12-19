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


parseOperands :: [[Char]] -> [Operand]
parseOperands [] = []
parseOperands operands = do
    let ret = Operand (head operands) 0 0

    let inner = parseOperands (tail operands)

    [ret] ++ inner


parseInstructionsInner :: [[[Char]]] -> [Instruction]
parseInstructionsInner [] = []
parseInstructionsInner lines = do
    let current = head lines
    let parts = break (\s -> last s /= ':') current
    let labels = [init l | l <- fst parts]
    let nonLabel = snd parts
    let operandStrings = tail nonLabel
    let command = head nonLabel

    let size = commandSize command

    let operands = parseOperands operandStrings
    let instruction = Instruction labels command operands 0

    [instruction] ++ parseInstructionsInner (tail lines)


parseInstructions :: [[[Char]]] -> [Instruction]
parseInstructions lines = parseInstructionsInner lines


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
                "jne"     -> 2
                "jl"      -> 2
                "jle"     -> 2
                "jg"      -> 2
                "jge"     -> 2
                "cmp"     -> 3 -- TODO: needs to be 2 if weird case
                "inc"     -> 2 -- TODO: 1 or 2 depending on operand
                "dec"     -> 2 -- TODO: 1 or 2 depending on operand


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


calculateOffsetsOperands :: [Operand] -> Int -> [Operand]
calculateOffsetsOperands [] _ = []
calculateOffsetsOperands operands offset = do
    let current = head operands

    let size = 8

    let newOperand = current {
        operandOffset = offset,
        operandSize = size
    }

    let inner = calculateOffsetsOperands (tail operands) (offset + size)

    [newOperand] ++ inner


calculateOffsetsInstructions :: [Instruction] -> Int -> [Instruction]
calculateOffsetsInstructions [] _ = []
calculateOffsetsInstructions instructions offset = do
    let current = head instructions

    let size = commandSize (command current)

    let newOperands = calculateOffsetsOperands (operands current)
                                               (offset + size)

    let instruction = current {
        instructionOffset = offset,
        operands = newOperands
    }

    let operandsSize = sum [operandSize o | o <- newOperands]
    let inner = calculateOffsetsInstructions (tail instructions)
                    (offset + size + operandsSize)

    [instruction] ++ inner


calculateOffsets :: [Section] -> [Section]
calculateOffsets sections = do
    let processSection sec = sec {
        instructions = calculateOffsetsInstructions (instructions sec) 0
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
    let sections2 = calculateOffsets sections

    (sections2, globals)
