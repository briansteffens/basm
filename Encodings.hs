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
    opExtension  = Nothing,
    modRmByte    = True
}


-- Placeholder for data encoding pseudo-instructions
dataEncoding = enc {
    mnemonic = DB
}


encodings = [

----ADD encodings--------------------------------------------------------------

    enc {
        mnemonic     = ADD,
        patterns     = [P_rm8, P_r8],
        primary      = 0x00
    },
    enc {
        mnemonic     = ADD,
        patterns     = [P_rm163264, P_r163264],
        primary      = 0x01,
        default32    = True
    },
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
        mnemonic     = ADD,
        patterns     = [R AL, P_imm8],
        primary      = 0x04
    },
    enc {
        mnemonic     = ADD,
        patterns     = [R RAX, P_imm1632],
        primary      = 0x05
    },
    enc {
        mnemonic     = ADD,
        patterns     = [P_rm8, P_imm8],
        primary      = 0x80,
        opExtension  = Just 0
    },
    enc {
        mnemonic     = ADD,
        patterns     = [P_rm163264, P_imm1632],
        primary      = 0x81,
        opExtension  = Just 0,
        default32    = True
    },
    enc {
        mnemonic     = ADD,
        patterns     = [P_rm163264, P_imm8],
        primary      = 0x83,
        opExtension  = Just 0
    },

----CALL encodings-------------------------------------------------------------

    enc {
        mnemonic     = CALL,
        patterns     = [P_rel1632],
        primary      = 0xE8,
        modRmByte    = False
    },

----CMP encodings--------------------------------------------------------------

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
        primary      = 0x3D,
        default32    = True,
        modRmByte    = False
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

----DEC encodings--------------------------------------------------------------

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

----INC encodings--------------------------------------------------------------

    enc {
        mnemonic     = INC,
        patterns     = [P_rm8],
        primary      = 0xFE,
        opExtension  = Just 0
    },
    enc {
        mnemonic     = INC,
        patterns     = [P_rm163264],
        primary      = 0xFF,
        default32    = True,
        opExtension  = Just 0
    },

----JE encodings---------------------------------------------------------------

    enc {
        mnemonic     = JE,
        patterns     = [P_rel8],
        modRmByte    = False,
        primary      = 0x74
    },
    enc {
        mnemonic     = JE,
        patterns     = [P_rel1632],
        modRmByte    = False,
        prefix0f     = True,
        primary      = 0x84
    },

----JG encodings---------------------------------------------------------------

    enc {
        mnemonic     = JG,
        patterns     = [P_rel8],
        modRmByte    = False,
        primary      = 0x7F
    },
    enc {
        mnemonic     = JG,
        patterns     = [P_rel1632],
        modRmByte    = False,
        prefix0f     = True,
        primary      = 0x8F
    },

----JMP encodings--------------------------------------------------------------

    enc {
        mnemonic     = JMP,
        patterns     = [P_rel8],
        modRmByte    = False,
        primary      = 0xEB
    },
    enc {
        mnemonic     = JMP,
        patterns     = [P_rel1632],
        modRmByte    = False,
        primary      = 0xE9
    },

----MOV encodings--------------------------------------------------------------

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

----POP encodings--------------------------------------------------------------

    enc {
        mnemonic     = POP,
        patterns     = [P_r6416],
        primary      = 0x58,
        registerAdd  = True,
        modRmByte    = False
    },
    enc {
        mnemonic     = POP,
        patterns     = [P_rm6416],
        primary      = 0x8F,
        opExtension  = Just 0
    },

----PUSH encodings-------------------------------------------------------------

    enc {
        mnemonic     = PUSH,
        patterns     = [P_r6416],
        primary      = 0x50,
        registerAdd  = True,
        modRmByte    = False
    },
    enc {
        mnemonic     = PUSH,
        patterns     = [P_imm1632],
        primary      = 0x68
    },
    enc {
        mnemonic     = PUSH,
        patterns     = [P_imm8],
        primary      = 0x6A
    },
    enc {
        mnemonic     = PUSH,
        patterns     = [P_rm6416],
        primary      = 0xFF,
        opExtension  = Just 6
    },

----RET encodings--------------------------------------------------------------

    enc {
        mnemonic     = RET,
        primary      = 0xC3
    },

----SETE encodings-------------------------------------------------------------

    enc {
        mnemonic     = SETE,
        patterns     = [P_rm8],
        prefix0f     = True,
        primary      = 0x94
    },

----SETNE encodings------------------------------------------------------------

    enc {
        mnemonic     = SETNE,
        patterns     = [P_rm8],
        prefix0f     = True,
        primary      = 0x95
    },

----SYSCALL encodings----------------------------------------------------------

    enc {
        mnemonic     = SYSCALL,
        prefix0f     = True,
        primary      = 0x05
    },

----TEST encodings-------------------------------------------------------------

    enc {
        mnemonic     = TEST,
        patterns     = [P_rm8, P_r8],
        primary      = 0x84
    },
    enc {
        mnemonic     = TEST,
        patterns     = [P_rm163264, P_r163264],
        primary      = 0x85,
        default32    = True
    },
    enc {
        mnemonic     = TEST,
        patterns     = [R AL, P_imm8],
        primary      = 0xA8
    },
    enc {
        mnemonic     = TEST,
        patterns     = [R RAX, P_imm1632],
        primary      = 0xA9
    },
    enc {
        mnemonic     = TEST,
        patterns     = [P_rm8, P_imm8],
        primary      = 0xF6,
        opExtension  = Just 0   -- Just 1 is an alias for this opcode
    },
    enc {
        mnemonic     = TEST,
        patterns     = [P_rm163264, P_imm1632],
        primary      = 0xF7,
        opExtension  = Just 0,  -- Just 1 is an alias for this opcode
        default32    = True
    },

----XOR encodings--------------------------------------------------------------

    enc {
        mnemonic     = XOR,
        patterns     = [P_rm8, P_r8],
        primary      = 0x30
    },
    enc {
        mnemonic     = XOR,
        patterns     = [P_rm163264, P_r163264],
        primary      = 0x31,
        default32    = True
    },
    enc {
        mnemonic     = XOR,
        patterns     = [P_r8, P_rm8],
        primary      = 0x32,
        reverseOpers = True
    },
    enc {
        mnemonic     = XOR,
        patterns     = [P_r163264, P_rm163264],
        primary      = 0x33,
        reverseOpers = True,
        default32    = True
    },
    enc {
        mnemonic     = XOR,
        patterns     = [R AL, P_imm8],
        primary      = 0x34
    },
    enc {
        mnemonic     = XOR,
        patterns     = [R RAX, P_imm1632],
        primary      = 0x35
    },
    enc {
        mnemonic     = XOR,
        patterns     = [P_rm8, P_imm8],
        primary      = 0x80,
        opExtension  = Just 6
    },
    enc {
        mnemonic     = XOR,
        patterns     = [P_rm163264, P_imm1632],
        primary      = 0x81,
        opExtension  = Just 6,
        default32    = True
    },
    enc {
        mnemonic     = XOR,
        patterns     = [P_rm163264, P_imm8],
        primary      = 0x83,
        opExtension  = Just 6
    }
    ]
