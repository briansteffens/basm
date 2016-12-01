%define something 123

section .text

global _start
_start:
    mov rax, 60
    mov rdi, something
    syscall
