section .data
    string_literal_0: db "hello", 0
    string_literal_1: db "el", 0

section .text
global _start
_start:
    push rbp
    mov rbp, rsp

    mov rax, string_literal_0
    mov rbx, string_literal_1
    sub rbx, rax
    mov rdi, rbx

    mov rax, 60
    syscall
