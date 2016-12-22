from assemble import assemble


regs64s = ['rax', 'rbx', 'rcx', 'rdx', 'rdi', 'rsi', 'rbp', 'rsp']
regs64x = ['r8',  'r9',  'r10', 'r11', 'r12', 'r13', 'r14', 'r15']
regs32  = ['eax', 'ebx', 'ecx', 'edx', 'edi', 'esi', 'ebp', 'esp']
regs16  = ['ax',  'bx',  'cx',  'dx',  'di',  'si',  'bp',  'sp' ]
regs8l  = ['al',  'bl',  'cl',  'dl']
regs8h  = ['ah',  'bh',  'ch',  'dh']

regs64 = regs64s + regs64x
regs8 = regs8l + regs8h

regs = regs64 + regs32 + regs16 + regs8


class Instruction(object):
    def __init__(self, command, operands):
        self.command = command
        self.operands = operands
        self.instruction = None
        self.assembled = None
        self.hex = None
        self.bin = None


INST_DIVIDER = Instruction(None, None)

instructions = []

#for regset in [regs64, regs32, regs16, regs8]:
#    for cmd in ['cmp', 'mov']:
#        for reg in regset:
#            instructions.append(Instruction(cmd, [reg, '7']))
#        instructions.append(INST_DIVIDER)
#
#for regset in [regs64, regs32, regs16, regs8]:
#    for cmd in ['inc', 'dec']:
#        for reg in regset:
#            instructions.append(Instruction(cmd, [reg]))
#        instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['al', 'ah']))
instructions.append(Instruction('add', ['[rax]', 'ah']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['ax', 'bx']))
instructions.append(Instruction('add', ['eax', 'ebx']))
instructions.append(Instruction('add', ['rax', 'rbx']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['[rax]', 'bx']))
instructions.append(Instruction('add', ['[rax]', 'ebx']))
instructions.append(Instruction('add', ['[rax]', 'rbx']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['ah', '[rax]']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['bx', '[rax]']))
instructions.append(Instruction('add', ['ebx', '[rax]']))
instructions.append(Instruction('add', ['rbx', '[rax]']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['al', '3']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['ax', '3']))
instructions.append(Instruction('add', ['eax', '3']))
instructions.append(Instruction('add', ['rax', '3']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['bl', '3']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['bx', '3']))
instructions.append(Instruction('add', ['ebx', '3']))
instructions.append(Instruction('add', ['rbx', '3']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add byte', ['[rax]', '3']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add word', ['[rax]', '3']))
instructions.append(Instruction('add dword', ['[rax]', '3']))
instructions.append(Instruction('add qword', ['[rax]', '3']))
instructions.append(INST_DIVIDER)

instructions.append(Instruction('add', ['rax', '[rax+7]']))
instructions.append(Instruction('add', ['rax', '[rax+rbx]']))
instructions.append(Instruction('add', ['rax', '[rax*2]']))
instructions.append(Instruction('add', ['rax', '[rax*2+7]']))
instructions.append(Instruction('add', ['rax', '[rax*2+rbx]']))
instructions.append(Instruction('add', ['rax', '[rax*2+rbx+7]']))
instructions.append(INST_DIVIDER)

for inst in instructions:
    if inst == INST_DIVIDER:
        continue

    inst.instruction = inst.command
    if len(inst.operands) > 0:
        inst.instruction += ' ' + ', '.join(inst.operands)

    inst.assembled = assemble(inst.instruction)

    inst.hex = [hex(a)[2:].zfill(2) for a in inst.assembled]
    inst.bin = [bin(a)[2:].zfill(8) for a in inst.assembled]

non_special = [i for i in instructions if i != INST_DIVIDER]

instruction_max = max([len(i.instruction) for i in non_special])
bytes_max = max([len(i.assembled) for i in non_special])
hex_max = 3 * bytes_max - 1

def pad(string, length):
    while len(string) < length:
        string += ' '

    return string

for inst in instructions:
    if inst == INST_DIVIDER:
        print()
        continue

    instruction = pad(inst.instruction, instruction_max)
    hex_out = pad(' '.join(inst.hex), hex_max)
    bin_out = ' '.join(inst.bin)

    line = '    '.join([instruction, hex_out, bin_out])

    print(line)
