section .data

    message: db "Greetings!", 10, 0

section .text

global loop_start
global _start:function
global loop_end:function, message:data
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
