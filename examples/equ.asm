section .data

    msg: db "Greetings!", 10
    msg_len: equ $-msg

section .text

global _start
_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, msg
    mov rdx, msg_len
    syscall

    mov rax, 60
    mov rdi, 0
    syscall
