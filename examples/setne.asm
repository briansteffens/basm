section .text

global _start
_start:
    setne bl

    mov rdi, rbx
    mov rax, 60
    syscall
