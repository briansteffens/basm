section .text

extern f

global _start
_start:
    mov rdi, 3
    call f

    mov rdi, rax
    mov rax, 60
    syscall
