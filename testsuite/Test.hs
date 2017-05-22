module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Definitions
import Lexer


commentTest :: Assertion
commentTest = assertEqual "Comments should not become tokens" expected actual
    where actual   = lexer "; a comment\nmov rax, 60; another\n; one more"
          expected = [Line {sourceCode = "; a comment",
                            lineNumber = 1,
                            tokens     = []},
                      Line {sourceCode = "mov rax, 60; another",
                            lineNumber = 2,
                            tokens     = [TCommand  MOV,
                                          TRegister RAX,
                                          TSymbol   Comma,
                                          TNumber   60]},
                      Line {sourceCode = "; one more",
                            lineNumber = 3,
                            tokens     = []}]


quoteTest :: Assertion
quoteTest = assertEqual "Quotes should handle escaping" expected $ lexer source
    where source   = "msg: db \"a \\\"quote\"\" here\\\"\"\"\", 10"
          expected = [Line {sourceCode = source,
                            lineNumber = 1,
                            tokens     = [TLabel   "msg",
                                          TCommand DB,
                                          TQuote   "a \"quote\" here\"\"",
                                          TSymbol  Comma,
                                          TNumber  10]}]


directiveTest :: Assertion
directiveTest = assertEqual "Parse directives" expected $ lexer source
    where source   = "global _start\n" ++
                     "global somefunc:function\n" ++
                     "extern printf"
          expected = [Line {sourceCode = "global _start",
                            lineNumber = 1,
                            tokens     = [TWord  "global",
                                          TWord  "_start"]},
                      Line {sourceCode = "global somefunc:function",
                            lineNumber = 2,
                            tokens     = [TWord  "global",
                                          TLabel "somefunc",
                                          TWord  "function"]},
                      Line {sourceCode = "extern printf",
                            lineNumber = 3,
                            tokens     = [TWord  "extern",
                                          TWord  "printf"]}]


main :: IO ()
main = defaultMainWithOpts [
        testCase "comment" commentTest,
        testCase "quote" quoteTest,
        testCase "directive" directiveTest
      ] mempty
