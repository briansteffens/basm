section .text

global _start
_start:
    cmp qword [rbp-8], 0
