%define sys_exit 60

section .data

    message: db "Hello, world!", 10

section .text

; Greetings

_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, message
    mov rdx, 14

    mov rax, 60
    mov rdi, 77         ; stuff and things
    syscall
