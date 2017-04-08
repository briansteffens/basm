section .text

global _start
_start:
    mov rbp, rsp
    sub rsp, 8

    mov qword [rbp-8], 7

    mov rax, 60
    mov rdi, [rbp-8]
    syscall
