section .text

# Greetings

_start:
    mov rax, 60

    # Hello
    mov rdi, 77
    syscall
