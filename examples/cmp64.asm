section .text

global _start
_start:
    cmp rax, 0

    mov rdi, rbx
    mov rax, 60
    syscall
