section .text

global _start
_start:
    mov rax, [rbx * 8 + rcx]
