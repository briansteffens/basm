section .text
global _start
_start:
    mov rbx, [rbp + 8 * rcx + 16]
