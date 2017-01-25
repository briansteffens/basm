module Main where

import Data.List

import qualified Definitions as D


data Encoding = Encoding {
    command  :: D.Command,
    operands :: [Pattern]
}


-- A description of the possible values of an operand, based on ref.x86asm.net
data Pattern = R8            -- An 8-bit register
             | RM8           -- An 8-bit register or memory address
             | R163264       -- A 16/32/64-bit register
             | RM163264      -- A 16/32/64-bit register or memory address
             | IMM8          -- An 8-bit immediate
             | IMM1632       -- A 16/32-bit immediate
             | IMM163264     -- A 16/32/64-bit immediate
             | R D.Registers -- A single register match
             deriving Show


-- Convenience function for creating encodings
define :: D.Command -> [Pattern] -> Encoding
define cmd opers =
    Encoding {
        command  = cmd,
        operands = opers
    }


-- Get a list of byte counts a given immediate can contain.
immBytes :: Pattern -> [Int]
immBytes IMM8      = [1]
immBytes IMM1632   = [2, 4]
immBytes IMM163264 = [2, 4, 8]


-- Check if a register is 8-bit.
is8 :: D.Registers -> Bool
is8 r = elem r D.registers8


-- Check if an operand can match the given pattern.
matchPattern :: D.Operand -> Pattern -> Bool

matchPattern (D.Register r) (R r2)          = r == r2

matchPattern (D.Register r) R8              = elem r D.registers8
matchPattern (D.Register r) RM8             = elem r D.registers8
matchPattern (D.Register r) R163264         = not (elem r D.registers8)
matchPattern (D.Register r) RM163264        = not (elem r D.registers8)

matchPattern (D.Address _ _ _ _) RM8        = True
matchPattern (D.Address _ _ _ _) RM163264   = True

matchPattern (D.Immediate (D.Literal l)) p  = elem (length l) (immBytes p)

matchPattern (D.Immediate (D.Symbol s _)) p = elem (D.sizeInt s) (immBytes p)

matchPattern _ _                            = False


-- Check if an instruction can be encoded using the given Encoding.
matchEncoding :: D.Instruction -> Encoding -> Bool
matchEncoding inst enc = do
    let instOpers = (D.operands inst)
    let encOpers = (operands enc)

    all (== True) ([D.command inst == command enc,
                    length instOpers == length encOpers] ++
                   zipWith matchPattern instOpers encOpers)


-- Find all Encodings that can encode the given instruction.
candidates :: D.Instruction -> [Encoding]
candidates inst = do
    let match enc = case matchEncoding inst enc of True  -> [enc]
                                                   False -> []

    concat (map match encodings)


encodings = [
    define D.ADC     [RM8     , R8       ],
    define D.ADC     [RM163264, R163264  ],
    define D.ADC     [R8      , RM8      ],
    define D.ADC     [R163264 , RM163264 ],
    define D.ADC     [R D.AL  , IMM8     ],
    define D.ADC     [R D.RAX , IMM1632  ],
    define D.ADC     [RM8     , IMM8     ],
    define D.ADC     [RM163264, IMM1632  ],
    define D.ADC     [RM163264, IMM8     ],

    define D.ADD     [RM8     , R8       ],
    define D.ADD     [RM163264, R163264  ],
    define D.ADD     [R8      , RM8      ],
    define D.ADD     [R163264 , RM163264 ],
    define D.ADD     [R D.AL  , IMM8     ],
    define D.ADD     [R D.RAX , IMM1632  ],
    define D.ADD     [RM8     , IMM8     ],
    define D.ADD     [RM163264, IMM1632  ],
    define D.ADD     [RM163264, IMM8     ],

    define D.AND     [RM8     , R8       ],
    define D.AND     [RM163264, R163264  ],
    define D.AND     [R8      , RM8      ],
    define D.AND     [R163264 , RM163264 ],
    define D.AND     [R D.AL  , IMM8     ],
    define D.AND     [R D.RAX , IMM1632  ],
    define D.AND     [RM8     , IMM8     ],
    define D.AND     [RM163264, IMM1632  ],
    define D.AND     [RM163264, IMM8     ],

    define D.CMP     [RM8     , R8       ],
    define D.CMP     [RM163264, R163264  ],
    define D.CMP     [R8      , RM8      ],
    define D.CMP     [R163264 , RM163264 ],
    define D.CMP     [R D.AL  , IMM8     ],
    define D.CMP     [R D.RAX , IMM1632  ],
    define D.CMP     [RM8     , IMM8     ],
    define D.CMP     [RM163264, IMM1632  ],
    define D.CMP     [RM163264, IMM8     ],

    define D.MOV     [RM8     , R8       ],
    define D.MOV     [RM163264, R163264  ],
    define D.MOV     [R8      , RM8      ],
    define D.MOV     [R163264 , RM163264 ],
    define D.MOV     [R8      , IMM8     ],
    define D.MOV     [R163264 , IMM163264],
    define D.MOV     [RM8     , IMM8     ],
    define D.MOV     [RM163264, IMM1632  ],

    define D.OR      [RM8     , R8       ],
    define D.OR      [RM163264, R163264  ],
    define D.OR      [R8      , RM8      ],
    define D.OR      [R163264 , RM163264 ],
    define D.OR      [R D.AL  , IMM8     ],
    define D.OR      [R D.RAX , IMM1632  ],
    define D.OR      [RM8     , IMM8     ],
    define D.OR      [RM163264, IMM1632  ],
    define D.OR      [RM163264, IMM8     ],

    define D.SBB     [RM8     , R8       ],
    define D.SBB     [RM163264, R163264  ],
    define D.SBB     [R8      , RM8      ],
    define D.SBB     [R163264 , RM163264 ],
    define D.SBB     [R D.AL  , IMM8     ],
    define D.SBB     [R D.RAX , IMM1632  ],
    define D.SBB     [RM8     , IMM8     ],
    define D.SBB     [RM163264, IMM1632  ],
    define D.SBB     [RM163264, IMM8     ],

    define D.SUB     [RM8     , R8       ],
    define D.SUB     [RM163264, R163264  ],
    define D.SUB     [R8      , RM8      ],
    define D.SUB     [R163264 , RM163264 ],
    define D.SUB     [R D.AL  , IMM8     ],
    define D.SUB     [R D.RAX , IMM1632  ],
    define D.SUB     [RM8     , IMM8     ],
    define D.SUB     [RM163264, IMM1632  ],
    define D.SUB     [RM163264, IMM8     ],

    define D.SYSCALL [                   ],

    define D.XOR     [RM8     , R8       ],
    define D.XOR     [RM163264, R163264  ],
    define D.XOR     [R8      , RM8      ],
    define D.XOR     [R163264 , RM163264 ],
    define D.XOR     [R D.AL  , IMM8     ],
    define D.XOR     [R D.RAX , IMM1632  ],
    define D.XOR     [RM8     , IMM8     ],
    define D.XOR     [RM163264, IMM1632  ],
    define D.XOR     [RM163264, IMM8     ]

    ]


main :: IO ()
main = do
    let inst = D.Instruction {
        D.source = "adc bl, cl",
        D.labelNames = [],
        D.command = D.ADC,
        D.sizeHint = D.NoSize,
        D.operands = [
            D.Register D.BL,
            D.Register D.CL
        ]
    }

    let c = candidates inst

    putStrLn (intercalate "\n" (map showEncoding c))


showEncoding :: Encoding -> String
showEncoding enc =
    show (command enc) ++ "\t" ++ intercalate "\t" (map show (operands enc))
