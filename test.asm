%define sys_exit 60

section .data

    message db "Hello, world!", 10

section .text

# Greetings

_start:
    mov rax, 60

    # Hello
    mov rdi, 77
    syscall
