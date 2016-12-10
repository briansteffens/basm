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


registers = [
    'rax', 'rbx', 'rcx', 'rdx', 'rdi', 'rsi', 'rbp', 'rsp',
    'r8',  'r9',  'r10', 'r11', 'r12', 'r13', 'r14', 'r15',
    'eax', 'ebx', 'ecx', 'edx', 'edi', 'esi', 'ebp', 'esp',
    'ax',  'bx',  'cx',  'dx',  'di',  'si',  'bp',  'sp',
    'al',  'bl',  'cl',  'dl',
    'ah',  'bh',  'ch',  'dh',
]

class Instruction(object):
    def __init__(self, command, operands):
        self.command = command
        self.operands = operands
        self.instruction = None
        self.assembled = None
        self.hex = None
        self.bin = None

instructions = []

for reg in registers:
    instructions.append(Instruction('inc', [reg]))

for inst in instructions:
    inst.instruction = inst.command
    if len(inst.operands) > 0:
        inst.instruction += ' ' + ', '.join(inst.operands)

    inst.assembled = assemble(inst.instruction)

    inst.hex = [hex(a)[2:].zfill(2) for a in inst.assembled]
    inst.bin = [bin(a)[2:].zfill(8) for a in inst.assembled]

instruction_max = max([len(i.instruction) for i in instructions])
bytes_max = max([len(i.assembled) for i in instructions])
hex_max = 3 * bytes_max - 1

def pad(string, length):
    while len(string) < length:
        string += ' '

    return string

for inst in instructions:
    instruction = pad(inst.instruction, instruction_max)
    hex_out = pad(' '.join(inst.hex), hex_max)
    bin_out = ' '.join(inst.bin)

    line = '    '.join([instruction, hex_out, bin_out])

    print(line)
