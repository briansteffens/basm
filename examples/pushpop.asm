section .text
global _start
_start:
    mov rax, 50
    push rax

    mov rax, 60
    pop rdi
    syscall
