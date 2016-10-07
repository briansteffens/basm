%define asdf 123

section .data

    message: db "Greetings!", 10

section .text

_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, message
    mov rdx, 5
    syscall

    mov rax, 60
    mov rdi, 123
    syscall
