section .text

global _start
_start:
    mov rax, 60
    mov rdi, 123
    jmp skip
    mov rdi, 321
skip:
    syscall
