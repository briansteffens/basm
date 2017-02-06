module Encodings where

import Data.Binary
import Data.List

import Definitions


enc = Encoding {
    mnemonic     = ADC,     -- TODO: None or something?
    patterns     = [],
    prefix       = Nothing,
    prefix0f     = False,
    primary      = 0x00,    -- TODO: better default?
    registerAdd  = False,
    secondary    = Nothing,
    opcodeExt    = 0,
    reverseOpers = False,
    default32    = False,
    opExtension  = Nothing
}


-- Placeholder for data encoding pseudo-instructions
dataEncoding = enc {
    mnemonic = DB
}


encodings = [
    enc {
        mnemonic     = ADD,
        patterns     = [P_r8, P_rm8],
        primary      = 0x02,
        reverseOpers = True
    },
    enc {
        mnemonic     = ADD,
        patterns     = [P_r163264, P_rm163264],
        primary      = 0x03,
        reverseOpers = True,
        default32    = True
    },
    enc {
        mnemonic     = CMP,
        patterns     = [P_rm8, P_r8],
        primary      = 0x38
    },
    enc {
        mnemonic     = CMP,
        patterns     = [P_rm163264, P_r163264],
        primary      = 0x39,
        default32    = True
    },
    enc {
        mnemonic     = CMP,
        patterns     = [P_r8, P_rm8],
        primary      = 0x3A,
        reverseOpers = True
    },
    enc {
        mnemonic     = CMP,
        patterns     = [P_r163264, P_rm163264],
        primary      = 0x3B,
        reverseOpers = True,
        default32    = True
    },
    enc {
        mnemonic     = CMP,
        patterns     = [R AL, P_imm8],
        primary      = 0x3C
    },
    enc {
        mnemonic     = CMP,
        patterns     = [R RAX, P_imm1632],
        primary      = 0x3D
    },
    enc {
        mnemonic     = CMP,
        patterns     = [P_rm8, P_imm8],
        primary      = 0x80,
        opExtension  = Just 7
    },
    enc {
        mnemonic     = CMP,
        patterns     = [P_rm163264, P_imm1632],
        primary      = 0x81,
        default32    = True,
        opExtension  = Just 7
    },
    enc {
        mnemonic     = CMP,
        patterns     = [P_rm163264, P_imm8],
        primary      = 0x83,
        opExtension  = Just 7
    },
    enc {
        mnemonic     = DEC,
        patterns     = [P_rm8],
        primary      = 0xFE,
        opExtension  = Just 1
    },
    enc {
        mnemonic     = DEC,
        patterns     = [P_rm163264],
        primary      = 0xFF,
        default32    = True,
        opExtension  = Just 1
    },
    enc {
        mnemonic     = JG,
        patterns     = [P_rel8],
        primary      = 0x7F
    },
    enc {
        mnemonic     = JG,
        patterns     = [P_rel1632],
        prefix0f     = True,
        primary      = 0x8F
    },
    enc {
        mnemonic     = MOV,
        patterns     = [P_rm8, P_r8],
        primary      = 0x88
    },
    enc {
        mnemonic     = MOV,
        patterns     = [P_rm163264, P_r163264],
        primary      = 0x89,
        default32    = True
    },
    enc {
        mnemonic     = MOV,
        patterns     = [P_r8, P_rm8],
        primary      = 0x8A
    },
    enc {
        mnemonic     = MOV,
        patterns     = [P_r163264, P_rm163264],
        primary      = 0x8B,
        default32    = True
    },
    enc {
        mnemonic     = MOV,
        patterns     = [P_r8, P_imm8],
        primary      = 0xB0,
        registerAdd  = True
    },
    enc {
        mnemonic     = MOV,
        patterns     = [P_r163264, P_imm163264],
        primary      = 0xB8,
        registerAdd  = True,
        default32    = True
    },
    enc {
        mnemonic     = MOV,
        patterns     = [P_rm8, P_imm8],
        primary      = 0xC6
    },
    enc {
        mnemonic     = MOV,
        patterns     = [P_rm163264, P_imm1632],
        primary      = 0xC7,
        default32    = True
    },
    enc {
        mnemonic     = SYSCALL,
        prefix0f     = True,
        primary      = 0x05
    }
    ]


--main :: IO ()
--main = do
--    let inst = Instruction {
--        source = "mov bl, cl",
--        labelNames = [],
--        command = MOV,
--        sizeHint = NoSize,
--        operands = [
--            Register BL,
--            Register CL
--        ]
--    }
--
--    let c = candidates inst
--
--    putStrLn (intercalate "\n" (map showEncoding c))
--
--
--showEncoding :: Encoding -> String
--showEncoding enc =
--    show (mnemonic enc) ++ "\t" ++ intercalate "\t" (map show (patterns enc))
