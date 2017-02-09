section .data

    message: db "Greetings!", 10, 0

section .text

_start:
    mov rdi, message
    xor rax, rax

loop_start:
    cmp byte [rdi + rax], 0
    je loop_end

    inc rax
    jmp loop_start

loop_end:
    mov rdi, rax
    mov rax, 60
    syscall
