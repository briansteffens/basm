%define asdf 123

section .text

_start:
    mov rax, 60
    mov rdi, 123
    syscall
