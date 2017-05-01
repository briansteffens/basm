section .text
global _start
_start:
    mov rax, 123
    xor rdx, rdx
    mov rbx, 10
    idiv rbx
    mov rdi, rax

    mov rax, 60
    syscall
