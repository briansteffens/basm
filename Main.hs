module Main where

import Data.List
import qualified Data.ByteString.Lazy as B

import qualified Parser as P
import qualified Elf as E


main :: IO ()
main = do
    let (sections, errors) = P.parse testFile

    putStrLn ("\n" ++ (intercalate "\n\n" (map P.showCodeSection sections)))
    putStrLn ("\nerrors:\n" ++ (intercalate "\n" (map P.showError errors)))

    let (bytes, debug) = E.assemble sections

    putStrLn debug
    B.writeFile "basm.o" bytes    


testFile =
    "; a comment\n" ++
    "section .data\n" ++
    "    message: db \"Greetings!\", 10\n" ++
    "section .text\n" ++
    "   _start:\n" ++
    "       mov rax, 1\n" ++
    "       mov rdi, 1\n" ++
    "       mov rsi, message\n" ++
    "       mov rdx, 11\n" ++
    "       syscall\n" ++
    "       mov rax, 60\n" ++
    "       mov rdi, 77\n" ++
    "       syscall"
