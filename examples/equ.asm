%define asdf 123

section .data

    test: equ 123

section .text

global _start
_start:
    mov rax, 60
    mov rdi, test
    syscall
