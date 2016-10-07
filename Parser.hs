module Parser where

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

extractGlobals :: [[[Char]]] -> ([[[Char]]], [[Char]])
extractGlobals [] = ([], [])
extractGlobals lines = do
    let current = head lines

    let isGlobal = (head current) == "global"

    let retLine = case isGlobal of False -> [current]
                                   True  -> []

    let retGlobal = case isGlobal of False -> []
                                     True  -> [current !! 1]

    let inner = extractGlobals (tail lines)
    (retLine ++ fst inner, retGlobal ++ snd inner)

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
                "%define" -> 1   -- TODO: hack

parse :: [Char] -> ([Section], [[Char]])
parse input = do
    let (parsedLines, globals) = extractGlobals .
                                 preprocess $ input

    (tail . parseSections $ parsedLines, globals)
