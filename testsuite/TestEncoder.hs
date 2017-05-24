module TestEncoder where

import Data.Binary
import Numeric
import Test.HUnit
import Test.Framework.Providers.HUnit

import Definitions
import qualified Lexer
import qualified Parser
import qualified Encoder


hexDigit :: String -> Word8
hexDigit str@[_, _] = case readHex str of
    [(res, "")] -> fromIntegral res
    _           -> error("Invalid hex value: " ++ str)
hexDigit str     = error("Invalid hex value: " ++ str)


hexDigits :: String -> [Word8]
hexDigits str = map hexDigit $ words str


run src expected = testCase src $ assertEqual src expect $ bytes
    where expect   = hexDigits expected
          lexed    = case Lexer.lexer src of
                         [line]  -> line
                         _       -> error("Lexer failure")
          parsed   = case Parser.instruction lexed of
                         Nothing -> error("Parser failure")
                         Just i  -> i
          encoding = Encoder.chooseEncoding parsed
          encoded  = Encoder.encodeInstruction parsed encoding
          bytes    = Encoder.encodedBytes encoded


tests = [
    run "mov rax, 0"
        "48 b8 00 00 00 00 00 00 00 00",

    run "mov rax, 60"
        "48 b8 3c 00 00 00 00 00 00 00",

    run "mov rax, 5000000"
        "48 b8 40 4b 4c 00 00 00 00 00",

    run "mov rax, rbx"
        "48 89 d8",

    run "mov ax, bx"
        "66 89 d8",

    run "mov ax, 15"
        "66 b8 0f 00"

    ]
