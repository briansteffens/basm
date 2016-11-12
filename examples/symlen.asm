section .data

    message: db "Greetings!", 10
    message_len: equ $-message

section .text

global _start
_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, message
    mov rdx, message_len
    syscall

    mov rax, 60
    mov rdi, 123
    syscall
