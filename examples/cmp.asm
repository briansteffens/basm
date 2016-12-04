section .text

global _start
_start:
    mov rdi, 3
    cmp rdi, 3
    je match

    mov rdi, 13

match:
    mov rax, 60
    syscall
