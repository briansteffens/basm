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
    default32    = False
}


-- Placeholder for data encoding pseudo-instructions
dataEncoding = enc {
    mnemonic = DATA
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
