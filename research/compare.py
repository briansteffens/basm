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


registers = ['rax', 'rbx', 'rcx', 'rdx', 'rdi', 'rsi', 'rbp', 'rsp',
             'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'r15']

class Combo(object):
    def __init__(self, left, right):
        self.left = left
        self.right = right
        self.instruction = None
        self.assembled = None
        self.hex = None
        self.bin = None

combos = []

for left in ['rax', 'rbx', 'r8']:
    for right in registers:
        combos.append(Combo(left, right))

for left in registers:
    combos.append(Combo(left, '7'))

for combo in combos:
    combo.instruction = 'cmp {}, {}'.format(combo.left, combo.right)
    combo.assembled = assemble(combo.instruction)

    combo.hex = [hex(a)[2:].zfill(2) for a in combo.assembled]
    combo.bin = [bin(a)[2:].zfill(8) for a in combo.assembled]

instruction_max = max([len(c.instruction) for c in combos])
bytes_max = max([len(c.assembled) for c in combos])
hex_max = 3 * bytes_max - 1

def pad(string, length):
    while len(string) < length:
        string += ' '

    return string

for combo in combos:
    instruction = pad(combo.instruction, instruction_max)
    hex_out = pad(' '.join(combo.hex), hex_max)
    bin_out = ' '.join(combo.bin)

    line = '    '.join([instruction, hex_out, bin_out])

    print(line)
