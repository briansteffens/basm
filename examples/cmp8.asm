section .text

global _start
_start:
    cmp al, 3

    mov rax, 60
    mov rdi, 0
    syscall
