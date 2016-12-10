section .text

global _start
_start:
    mov rdi, 1
    inc rdi
    inc rdi

    mov rax, 60
    syscall
