import sys
import subprocess

def assemble(instruction):
    rendered = 'section .text\nglobal _start\n_start:\n' + instruction

    with open('file.asm', 'w') as f:
        f.write(rendered)

    subprocess.call('nasm -f elf64 -O0 file.asm -o file.o', shell=True)

    readelf = subprocess.check_output('readelf -a file.o', shell=True)
    readelf_lines = str(readelf, 'utf-8').split('\n')
    for i in range(len(readelf_lines)):
        if '.text' in readelf_lines[i]:
            line = readelf_lines[i + 1]
            size = int(line.split()[0], 16)
            break

    with open('file.o', 'rb') as f:
        return f.read()[0x180:][:size]


if __name__ == '__main__':
    instruction = ' '.join(sys.argv[1:])
    assembled = assemble(instruction)
    hex_out = ' '.join([hex(a)[2:].zfill(2) for a in assembled])
    bin_out = ' '.join([bin(a)[2:].zfill(8) for a in assembled])

    print('    '.join([instruction, hex_out, bin_out]))
