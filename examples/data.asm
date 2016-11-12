section .data

    message: db "Greetings!", 10

section .text

_start:
    mov rax, 60
    mov rdi, 123
    syscall
