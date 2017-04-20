section .data

    value: dq 6

section .text

global _start
_start:
    mov rcx, [value]
    inc rcx
    mov [value], rcx

    mov rax, 60
    mov rdi, [value]
    syscall
