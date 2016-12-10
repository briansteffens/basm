section .text

global _start
_start:
    mov rdi, 7
    dec rdi
    dec rdi

    mov rax, 60
    syscall
