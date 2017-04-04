section .text

global _start
_start:
    sete bl

    mov rdi, rbx
    mov rax, 60
    syscall
