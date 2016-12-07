section .text

global _start
_start:
    mov rdi, 3
    cmp rdi, 3
    jge match

    mov rdi, 13

match:
    mov rax, 60
    syscall
