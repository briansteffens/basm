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
    --"   message:\n" ++
    --"   thing: db   \"here is a message \\\\ \\\" with quote\"\n" ++
    "# look more:\n" ++
    "section .text\n" ++
    "   _start:\n" ++
    "       mov rax, 60#a comment\n" ++
    "       mov rdi, 77\n" ++
    --"       add qword rbx, [rcx + 2*rcx+rdx]\n" ++
    "       syscall"
