section .data

    message: db "Greetings!", 10, 0

section .text

global _start
_start:
    mov rax, 60
bar:
    mov rdi, 7
    syscall
