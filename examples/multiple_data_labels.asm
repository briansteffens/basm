section .data

    a: dq 0
    b: dq 0

section .text

global _start
_start:
    mov qword [a], 3
    mov qword [b], 7

    mov rax, 60
    mov rdi, [a]
    syscall
