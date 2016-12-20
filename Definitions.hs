module Definitions where

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

registersStandard64 = ["rax", "rbx", "rcx", "rdx", "rdi", "rsi", "rbp", "rsp"]
registersExtended64 = ["r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15"]
registers32         = ["eax", "ebx", "ecx", "edx", "edi", "esi", "ebp", "esp"]
registers16         = ["ax",  "bx",  "cx",  "dx",  "di",  "si",  "bp",  "sp" ]
registersLow8       = ["al",  "bl",  "cl",  "dl"]
registersHigh8      = ["ah",  "bh",  "ch",  "dh"]

registers64 = registersStandard64 ++ registersExtended64
registers8 = registersLow8 ++ registersHigh8

registersAll = registers64 ++ registers32 ++ registers16 ++ registers8

-- Get the number of bits in the given register
bitsInRegister :: [Char] -> Int
bitsInRegister register
    | elem register registers64 = 64
    | elem register registers32 = 32
    | elem register registers16 = 16
    | elem register registers8  = 8

-- Check if a string matches a known register
isRegister :: [Char] -> Bool
isRegister register = elem register registersAll

-- Returns a flag indicating if the register is one of the new 64-bit ones
isExtendedRegister :: [Char] -> Int
isExtendedRegister register =
    if elem register registersExtended64 then 1 else 0

-- Special case where if the first operand is rax and the second operand is an
-- immediate, the instruction renders differently
-- TODO: This should also apply to eax/ax/al
isWeirdCmpCase :: Instruction -> Bool
isWeirdCmpCase inst = do
    let second = text (last (operands inst))
    let first = text (head (operands inst))

    first == "rax" && not (isRegister second)
