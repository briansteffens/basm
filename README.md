basm
====

[![Build Status](https://travis-ci.org/briansteffens/basm.svg?branch=master)](https://travis-ci.org/briansteffens/basm)

An x86-64 assembler.

As this is primarily used by [bshift](https://github.com/briansteffens/bshift),
it only implements the instructions and features needed by bshift.



# Downloading and compiling

You'll need git, a haskell compiler, and cabal:

```bash
git clone https://github.com/briansteffens/basm
cd basm
cabal build
```





# Usage

Consider the following hello world style program (examples/hello.asm):

```asm
section .data

    message: db "Greetings!", 10

section .text

global _start
_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, message
    mov rdx, 11
    syscall

    mov rax, 60
    mov rdi, 0
    syscall
```

To assemble, link, and run it:

```bash
dist/build/basm/basm examples/hello.asm
ld examples/hello.o
./a.out
```
