section .text

global _start
_start:
    call add_func

    mov rdi, rax
    mov rax, 60
    syscall

add_func:
    mov rax, 3
    ret
