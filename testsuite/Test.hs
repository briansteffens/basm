module Main where

import Test.Framework

import qualified TestLexer as TestLexer
import qualified TestEncoder as TestEncoder


main :: IO ()
main = defaultMainWithOpts allTests mempty
    where allTests = concat [TestLexer.tests, TestEncoder.tests]
