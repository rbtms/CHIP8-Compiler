import sys

def loadProgram(path):
    file = open(path, 'rb')
    file = list(file.read())
    program = []

    for i in range(0, len(file), 2):
        if i+1 < len(file):
            program.append(file[i]*256+file[i+1])
        else:
            program.append(file[i])

    return program

def splitOP(op):
    d = (op & 0xF000) >> 12
    c = (op & 0x0F00) >> 8
    b = (op & 0x00F0) >> 4
    a = op & 0x000F
    nnn, nn = op&0x0FFF, op&0x00FF

    return d, c, b, a, nnn, nn

def dissasemble(intr, op):
    addr = intr.ip-2
    I = intr.I
    r = intr.reg
    d, c, b, a, nnn, nn = splitOP(op)
    s = ''

    # 0x00E0
    if op == 0x00E0: s = '  0x00E0: Clear screen'
    # 0x00EE
    elif op == 0x00EE: s = '  0x00EE: Return from subroutine'
    # 0xNNN
    elif d == 0x0: s = '0x0NNN'

    # 0x1NNN
    elif d == 0x1: s = '0x1NNN: Jump to ' + hex(nnn)
    # 0x2NNN
    elif d == 0x2: s = '0x2NNN: Execute subroutine ' + hex(nnn)
    # 0x3XNN
    elif d == 0x3: s = '0x3XNN: Skip next if V' + str(c) + '('+str(intr.getReg(c))+') == ' + str(nn)
    # 0x4XNN
    elif d == 0x4: s = '0x4XNN: Skip next if V' + str(c) + '('+str(intr.getReg(c)) + ') != ' + str(nn)
    # 0x5XY0
    elif d == 0x5: s = '0x5XNN: Skip next if V' + str(c) + '('+str(intr.getReg(c))+') != V' + str(b) + '('+str(intr.getReg(b))+')'

    # 0x6XNN
    elif d == 0x6: s = '0x6XNN: V' + str(c) + '('+str(intr.getReg(c)) + ') = ' + str(nn)
    # 0x7XNN
    elif d == 0x7: s = '0x7XNN: V' + str(c) + '('+str(intr.getReg(c)) + ') += ' + str(nn)

    # 0x8XY(01234567E)
    elif d == 0x8:
        if   a == 0x0: s = '0x8XY0: V' + str(c) + '('+str(intr.getReg(c)) + ') = V'   + str(b) + '('+str(intr.getReg(b)) + ')'
        elif a == 0x1: s = '0x8XY1: V' + str(c) + '('+str(intr.getReg(c)) + ') |= V'  + str(b) + '('+str(intr.getReg(b)) + ')'
        elif a == 0x2: s = '0x8XY2: V' + str(c) + '('+str(intr.getReg(c)) + ') &= V'  + str(b) + '('+str(intr.getReg(b)) + ')'
        elif a == 0x3: s = '0x8XY3: V' + str(c) + '('+str(intr.getReg(c)) + ') ^= V'  + str(b) + '('+str(intr.getReg(b)) + ')'
        elif a == 0x4: s = '0x8XY4: V' + str(c) + '('+str(intr.getReg(c)) + ') += V'  + str(b) + '('+str(intr.getReg(b)) + ')'
        elif a == 0x5: s = '0x8XY5: V' + str(c) + '('+str(intr.getReg(c)) + ') -= V'  + str(b) + '('+str(intr.getReg(b)) + ')'
        elif a == 0x6: s = '0x8XY6: V' + str(c) + '('+str(intr.getReg(c)) + ') >>= V' + str(b) + '('+str(intr.getReg(b)) + ')'
        elif a == 0x7: s = '0x8XY7: V' + str(c) + '('+str(intr.getReg(c)) + ') = V'   + str(b) + '('+str(intr.getReg(b)) + ')' + ' - V' + str(c) + '('+str(intr.getReg(c)) + ')'
        elif a == 0xE: s = '0x8XYE: V' + str(c) + '('+str(intr.getReg(c)) + ') <<= V' + str(b) + '('+str(intr.getReg(b)) + ')'
        else:
            s = 'DATA'

    # 0x9XY0
    elif d == 0x9: s = '0x9XY0: Skip next if V' + str(c) + '('+str(intr.getReg(c)) + ')' + ' != V' + str(b) + '('+str(intr.getReg(b)) + ')'

    # 0xANNN
    elif d == 0xA: s = '0xANNN: I' + '('+hex(intr.I) + ')' + ' = ' + hex(nnn)
    # 0xBNNN
    elif d == 0xB: s = '0xBNNN: Jump to ' + hex(nnn) + ' + V0(' + str(intr.getReg(0)) + ')'

    # 0xCXNN
    elif d == 0xC: s = '0xCXNN: V' + str(c) + '('+str(intr.getReg(c)) + ')' + ' = rndint with mask ' + str(nn)

    # 0xDXYN
    elif d == 0xD: s = '0xDXYN: Draw sprite at (V' + str(c) + '('+str(intr.getReg(c)) + ')' + ', ' + 'V' + str(b)\
                       + '('+str(intr.getReg(b)) + ')) with height ' + str(a)

    elif d == 0xE:
        # 0xEX9E
        if nn == 0x9E: s = '0xEX9E: Skip next if V' + hex(c) + '('+str(intr.getReg(c)) + ')' + ' pressed'
        # 0xEXA1
        elif nn == 0xA1: s = '0xEX9E: Skip next if V' + hex(c) + '('+str(intr.getReg(c)) + ')' + ' not pressed'
        else:
            raise ValueError('0x0NNN')

    elif d == 0xF:
        # 0xFX07
        if nn == 0x07: s = '0xFX07: Store TIMER_DELAY on V' + str(c) + '('+str(intr.getReg(c)) + ')'
        # 0xFX0A
        elif nn == 0x0A: s = '0xFX0A: Wait and store keypress on V' + str(c) + '('+str(intr.getReg(c)) + ')'
        # 0xFX15
        elif nn == 0x15: s = '0xFX15: Set TIMER_DELAY to V' + str(c) + '('+str(intr.getReg(c)) + ')'
        # 0xFX18
        elif nn == 0x18: s = '0xFX18: Set TIMER_SOUND to V' + str(c) + '('+str(intr.getReg(c)) + ')'
        # 0xFX1E
        elif nn == 0x1E: s = '0xFX1E: I' + '('+hex(intr.I) + ')' + ' += V' + str(c) + '('+str(intr.getReg(c)) + ')'
        # 0xFX29
        elif nn == 0x29: s = '0xFX29: I = @Sprite(V' + str(c)  + '('+str(intr.getReg(c)) + ')' + ')'
        #
        # 0xFX33
        elif nn == 0x33: s = '0xFX33: Store BCD of V' + str(c) + '('+str(intr.getReg(c)) + ')'+ ' on I('+hex(intr.I) + ' - ' + hex(intr.I+2) + ')'
        # 0xFX55
        elif nn == 0x55: s = '0xFX55: Dump from V0 to V' + str(c) + '('+str(intr.getReg(c)) + ')' + ' at I' + '('+hex(intr.I) + ')'
        # 0xFX65
        elif nn == 0x65: s = '0xFX65: Load I ' + '('+hex(intr.I) + ' - ' + hex(intr.I+c) + ')' + ' on VX'
        else:
            s = 'DATA'

    return ' ' + hex(addr) + ' | ' + '(' + hex(op) + ') ' + s

if __name__ == '__main__':
    if len(sys.argv) < 2:
        raise ValueError('Not enough arguments.')
    else:
        path = sys.argv[1]
        program = loadProgram(path)#[:10]

        print('\n' + path)
        print('--------------------\n')
        print('PROGRAM:\n---------------------\n\n', list(map(hex, program)))
        print('\nDISSASEMBLY:\n-----------------------\n')

        for i, op in enumerate(program):
            print( dissasemble(0x200+1, op) )
