import sys
import screen
import dissasembler
import time
import winsound
import random

class Interpreter:
    def __init__(self, program, verbose=True, headless=False):
        self.MEM_SIZE    = 0x1000 # 4096
        self.REG_SIZE    = 16
        self.START_ADDR  = 0x200
        self.PROGRAM_LEN = len(program)
        self.START_TIME  = time.time()

        self.CHAR_SPRITES = [
            0xf0, 0x90, 0x90, 0x90, 0xf0, # 0
            0x20, 0x60, 0x20, 0x20, 0x70, # 1
            0xf0, 0x10, 0xf0, 0x80, 0xf0, # 2
            0xf0, 0x10, 0xf0, 0x10, 0xf0, # 3
            0x90, 0x90, 0xf0, 0x10, 0x10, # 4
            0xf0, 0x80, 0xf0, 0x10, 0xf0, # 5
            0xf0, 0x80, 0xf0, 0x90, 0xf0, # 6
            0xf0, 0x10, 0x20, 0x40, 0x40, # 7
            0xf0, 0x90, 0xf0, 0x90, 0xf0, # 8
            0xf0, 0x90, 0xf0, 0x10, 0xf0, # 9
            0xf0, 0x90, 0xf0, 0x90, 0x90, # A
            0xe0, 0x90, 0xe0, 0x90, 0xe0, # B
            0xf0, 0x80, 0x80, 0x80, 0xf0, # C
            0xe0, 0x90, 0x90, 0x90, 0xe0, # D
            0xf0, 0x80, 0xf0, 0x80, 0xf0, # E
            0xf0, 0x80, 0xf0, 0x80, 0x80  # F
        ]

        self.tickDelay  = 0.0016
        self.isShowDissasembly = verbose
        self.isWaitKeypress = False
        self.waitReg = None

        self.mem   = [ 0 for _ in range(self.MEM_SIZE) ]
        self.reg   = [ 0 for _ in range(self.REG_SIZE) ]
        self.stack = []
        self.I     = 0
        self.ip    = self.START_ADDR

        self.timerDelay = 0
        self.timerSound = 0

        self.tickN = 0
        self.tLastTick = self.START_TIME
        self.tLastTimerDec  = self.START_TIME

        self.program = program
        self.loadSpriteChars()
        self.loadProgram(program)

        self.screen = screen.Screen(headless=headless)

    def loadSpriteChars(self):
        for i, n in enumerate(self.CHAR_SPRITES):
            self.setMem(i, n)

    def loadProgram(self, program):
        for i, n in enumerate(program):
            self.setMem(self.ip+i, n)

    def getMem(self, addr):
        if addr >= 0 and addr < self.MEM_SIZE:
            return self.mem[addr]
        else:
            raise ValueError('getMem: Invalid mem address.')

    def setMem(self, addr, val):
        if addr >= 0 and addr < self.MEM_SIZE:
            self.mem[addr] = val
        else:
            raise ValueError('setMem: Invalid mem address.')

    def getReg(self, regN):
        if regN >= 0 and regN < self.REG_SIZE:
            return self.reg[regN]
        else:
            raise ValueError('getReg: Invalid register.')

    def setReg(self, regN, n):
        if regN >= 0 and regN < self.REG_SIZE:
            self.reg[regN] = n
        else:
            raise ValueError('setReg: Invalid register.')

    def beep(self, ms):
        winsound.Beep(500, ms)

    def checkInput(self):
        code = self.screen.getChar()

        if code is not None:
            if code == -1:
                self.ip = -1
            # Key up
            elif code == -2:
                if self.tickDelay > 0.005:
                    self.tickDelay -= 0.005
            # Key down
            elif code == -3:
                if self.tickDelay < 0.995:
                    self.tickDelay += 0.005
            elif code == -4:
                self.isShowDissasembly = not self.isShowDissasembly
            elif self.isWaitKeypress:
                self.processKeypress(code)

    def decTimers(self):
        if self.timerDelay > 0 or self.timerSound > 0:
            # Decrement timers
            if time.time() - self.tLastTimerDec > 1/60:
                if self.timerDelay > 0:
                    self.timerDelay -= 1
                if self.timerSound > 0:
                    self.timerSound -= 1

                self.tLastTimerDec = time.time()

    #
    # OPCODES -------------------------------------------------------------------------
    #

    # 0x00E0: Clear screen
    def OP_cls(self):
        self.screen.clear()

    # 0x00NNN: Execute machine code
    def OP_execMachineCode(self, nnn):
        raise ValueError('0x0NNN: Execute machine code')

    # 0x00EE: Return from subroutine
    def OP_returnSub(self):
        if self.stack:
            self.ip = self.stack.pop()
        else:
            raise ValueError('returnSub: Empty stack.')

    # 0x1NNN: Jump to address nnn
    def OP_jmp(self, nnn):
        self.ip = nnn

    # 0x2NNN: Execute subroutine at address nnn
    def OP_execSub(self, nnn):
        if len(self.stack) < 200:
            self.stack.append(self.ip)
            self.ip = nnn
        else:
            raise ValueError('0x2NNN: Stack full.')

    # 0x3XNN: Skip next instruction if VX == nn
    def OP_skipEq(self, x, nn):
        if self.getReg(x) == nn:
            self.ip += 2

    # 0x4XNN: Skip next instruction if VX != nn
    def OP_skipNeq(self, x, nn):
        if self.getReg(x) != nn:
            self.ip += 2

    # 0x5XY0: skip next instruction if VX == VY
    def OP_skipEqXY(self, x, y):
        if self.getReg(x) == self.getReg(y):
            self.ip += 2

    # 0x6XNN: Store nn in VX
    def OP_storeVX(self, x, nn):
        self.setReg(x, nn)

    # 0x7XNN: Store VX+nn in VX
    def OP_addVX(self, x, nn):
        res = self.getReg(x)+nn

        self.setReg(x, res%256)

    # 0x8XY0: Move value in VY to VX
    def OP_moveVYtoVX(self, x, y):
        self.setReg(x, self.getReg(y))

    # 0x8XY1: VX |= VY
    def OP_or(self, x, y):
        res = self.getReg(x) | self.getReg(y)
        self.setReg(x, res)

    # 0x8XY2: VX &= VY
    def OP_and(self, x, y):
        res = self.getReg(x) & self.getReg(y)
        self.setReg(x, res)

    # 0x8XY3: VX ^= VY
    def OP_xor(self, x, y):
        res = self.getReg(x) ^ self.getReg(y)
        self.setReg(x, res)

    # 0x8XY4: VX += VY
    def OP_addVYtoVX(self, x, y):
        res = self.getReg(x) + self.getReg(y)

        # Set VF to carry
        self.setReg(0xF, 1 if res > 0xFF else 0)

        self.setReg(x, res%256)

    # 0x8XY5: VX -= VY
    # TODO: Check if VF is set when there is a borrow or not
    def OP_subVYfromVX(self, x, y):
        res = self.getReg(x) - self.getReg(y)

        # Set VF to borrow
        self.setReg(0xF, 0 if res < 0x00 else 1)

        self.setReg(x, res%256)

    # 0x8XY6: Right shift
    # VF = least significant bit of VX prior to the shift
    # VX = VY >> 1
    # TODO: Check if its y or x
    def OP_storeVYrshiftVX(self, x, y):
        self.setReg(0xF, self.getReg(y) & 1)
        self.setReg(x, self.getReg(y) >> 1)

    # 0x8XY7: VX = VY - VX
    # TODO: Check if VF is set when there is a borrow or not
    def OP_setVXsubVY_VX(self, x, y):
        res = self.getReg(y) - self.getReg(x)

        # Set VF to borrow
        self.setReg(0xF, 0 if res < 0x00 else 1)

        self.setReg(x, res%256)

    # 0x8XYE: Left shift
    # VF = most significant bit of VX prior to the shift
    # VX = VY << 1
    # TODO: Check if its y or x
    def OP_storeVYlshiftVX(self, x, y):
        self.setReg(0xF, self.getReg(y) >> 7)
        self.setReg(x, (self.getReg(y) << 1)%256)

    # 0x9XY0: Skip next instruction if VX != VY
    def OP_skipNeqXY(self, x, y):
        if self.getReg(x) != self.getReg(y):
            self.ip += 2

    # 0xANNN: Store nnn on I
    def OP_setItoNNN(self, nnn):
        self.I = nnn

    # 0xBNNN: Jump to nnn + V0
    def OP_jmpNNN_V0(self, nnn):
        self.ip = nnn + self.getReg(0x0)

    def OP_setRandomMaskVX(self, x, nn):
        rand = random.randint(0, nn)
        self.setReg(x, rand)

    # 0xDXYN: Draw a sprite on (vx, vy) with width=8 and height=n+1
    # each row is stored in (I+i)
    # VF is set to 1 if any pixel is flipped to 0
    def OP_drawSprite(self, x, y, n):
        vx = self.getReg(x)
        vy = self.getReg(y)
        onToOff = False

        # Dont draw if the sprites clip out of the screen
        for rowN in range(n):
            m = self.getMem(self.I + rowN)

            for shiftN in range(8):
                b = (m >> (7-shiftN)) & 1

                _x = (vx + shiftN)%self.screen.W
                _y = (vy + rowN)%self.screen.H

                hasFlipped = self.screen.drawPixel(_x, _y, b)

                # Set VF
                if hasFlipped: onToOff = True

        # Set VF
        self.setReg(0xF, 0x01 if onToOff else 0x00)
        self.screen.refresh()

    # 0xEX9E: Skip if the key on x is pressed
    def OP_skipPressed(self, x):
        key = self.getReg(x)

        if self.screen.isKeyPressed(key):
            self.ip += 2

    # 0xEXA1: Skip if the key on x is not pressed
    def OP_skipNotPressed(self, x):
        key = self.getReg(x)

        if not self.screen.isKeyPressed(key):
            self.ip += 2

    # 0xFX07: Store in VX the value in the delay timer
    def OP_storeDelay(self, x):
        self.setReg(x, self.timerDelay)

    # 0xFX0A: Wait for a keypress and store the pressed key on VX
    def OP_waitKeypress(self, x):
        self.isWaitKeypress = True
        self.waitReg = x

        # Decrement IP so that it doesnt jump out of the loop
        self.ip -= 2

    def processKeypress(self, code):
        self.isWaitKeypress = False

        # Quit with q/Q
        if code >= 0x00:
            self.setReg(self.waitReg, code)

            # Jump to the next instruction after the decrement
            self.ip += 2

    # 0xFX15: Set the delay timer to the value of VX
    def OP_setDelayTimer(self, x):
        self.timerDelay = self.getReg(x)

    # 0xFX18: Set the sound timer to the value of VX
    def OP_setSoundTimer(self, x):
        self.timerSound = self.getReg(x)
        self.beep(self.timerSound*10)

    # 0xFX1E: Add value in VX to I
    def OP_addVXtoI(self, x):
        self.I += self.getReg(x)

    # 0xFX29: Set I to the memory address where the sprite VX is stored
    def OP_setItoSpriteMemAddr(self, x):
        vx = self.getReg(x)
        self.I = vx*5

    # 0xFX33: Store the BCD of VX on I - I+2
    def OP_storeBCD(self, x):
        vx = self.getReg(x)

        self.setMem(self.I, int((vx - vx%100)/100))
        self.setMem(self.I+1, int((vx%100 - vx%10)/10))
        self.setMem(self.I+2, vx%10)

    # 0xFX55: Dump the contents of V0 - VX on memory
    def OP_dump(self, x):
        for i in range(x+1):
            self.setMem(self.I + i, self.getReg(i))

        self.I += x + 1

    # 0xFX65: Load the contents of memory onto V0 - VX
    def OP_load(self, x):
        for i in range(x+1):
            self.setReg(i, self.getMem(self.I + i))

        self.I += x + 1

    def processOpcode(self):
        op = self.mem[self.ip]*256 + self.mem[self.ip+1]
        self.ip += 2

        # Parse opcode
        d, c, b, a = (op&0xF000)>>12, (op&0x0F00)>>8, (op&0x00F0)>>4, op&0x000F
        nnn, nn = op&0x0FFF, op&0x00FF

        # Print current instruction
        if self.isShowDissasembly:
            print( dissasembler.dissasemble(self, op) )

        # 0x00E0
        if op == 0x00E0: self.OP_cls()
        # 0x00EE
        elif op == 0x00EE: self.OP_returnSub()
        # 0xNNN
        elif d == 0x0: self.OP_execMachineCode(nnn)

        # 0x1NNN
        elif d == 0x1: self.OP_jmp(nnn)
        # 0x2NNN
        elif d == 0x2: self.OP_execSub(nnn)
        # 0x3XNN
        elif d == 0x3: self.OP_skipEq(c, nn)
        # 0x4XNN
        elif d == 0x4: self.OP_skipNeq(c, nn)
        # 0x5XY0
        elif d == 0x5: self.OP_skipEqXY(c, b)

        # 0x6XNN
        elif d == 0x6: self.OP_storeVX(c, nn)
        # 0x7XNN
        elif d == 0x7: self.OP_addVX(c, nn)

        # 0x8XY(01234567E)
        elif d == 0x8:
            if   a == 0x0: self.OP_moveVYtoVX(c, b)
            elif a == 0x1: self.OP_or(c, b)
            elif a == 0x2: self.OP_and(c, b)
            elif a == 0x3: self.OP_xor(c, b)
            elif a == 0x4: self.OP_addVYtoVX(c, b)
            elif a == 0x5: self.OP_subVYfromVX(c, b)
            elif a == 0x6: self.OP_storeVYrshiftVX(c, b)
            elif a == 0x7: self.OP_setVXsubVY_VX(c, b)
            elif a == 0xE: self.OP_storeVYlshiftVX(c, b)
            else:
                raise ValueError('Invalid opcode: 0x8___')

        # 0x9XY0
        elif d == 0x9: self.OP_skipNeqXY(c, b)

        # 0xANNN
        elif d == 0xA: self.OP_setItoNNN(nnn)
        # 0xBNNN
        elif d == 0xB: self.OP_jmpNNN_V0(nnn)

        # 0xCXNN
        elif d == 0xC: self.OP_setRandomMaskVX(c, nn)

        # 0xDXYN
        elif d == 0xD:
            self.OP_drawSprite(c, b, a)

        elif d == 0xE:
            # 0xEX9E
            if nn == 0x9E:
                self.OP_skipPressed(c)
            # 0xEXA1
            elif nn == 0xA1:
                self.OP_skipNotPressed(c)
            else:
                raise ValueError('0x0NNN')

        elif d == 0xF:
            # 0xFX07
            if nn == 0x07:
                self.OP_storeDelay(c)
            # 0xFX0A
            elif nn == 0x0A:
                self.OP_waitKeypress(c)
            # 0xFX15
            elif nn == 0x15:
                self.OP_setDelayTimer(c)
            # 0xFX18
            elif nn == 0x18:
                self.OP_setSoundTimer(c)
            # 0xFX1E
            elif nn == 0x1E:
                self.OP_addVXtoI(c)
            # 0xFX29
            elif nn == 0x29:
                self.OP_setItoSpriteMemAddr(c)
            # 0xFX33
            elif nn == 0x33:
                self.OP_storeBCD(c)
            elif nn == 0x55:
                self.OP_dump(c)
            # 0xFX65
            elif nn == 0x65:
                self.OP_load(c)
            else:
                print('\n ERROR: Invalid opcode: 0xF___')
                self.ip = -1

    def run(self):
        # 50 - 65 ticks/s
        while self.ip >= 0x200 and self.ip < self.MEM_SIZE and self.ip-0x200 < self.PROGRAM_LEN:
            if time.time() - self.tLastTick > self.tickDelay:
                self.tLastTick = time.time()

                # Process a new opcode
                if not self.isWaitKeypress:
                    self.processOpcode()

                self.checkInput()
                self.decTimers()

                self.tickN += 1

    def printState(self):
        print('\nEND STATE')
        print('------------------------------------------------------')
        print('IP      : ', hex(self.ip))
        print()
        #print('PROGRAM : ')

        #for i in range(int(len(self.program)/20)+1):
        #    for j in range(20):
        #        if i*20+j < self.PROGRAM_LEN:
        #            print( ' ' + hex(self.program[i*20+j]).rjust(4), end='')
        #    if i*18 < len(self.program): print()

        print('LEN     : ', len(self.program), '(0x200-' + hex(0x200+len(self.program)) + ')')
        print('REG     : ', self.reg)
        print('STACK   : ', list(map(hex, self.stack)))
        print('I       : ', hex(self.I))
        print('TIMERS  : ', (self.timerDelay, self.timerSound))
        print('TICK N  : ', self.tickN)
        print()
        print('Run time    : ', time.time() - self.START_TIME, 's')

def loadProgram(path):
    file = open(path, 'rb')
    l = list(file.read())
    program = []

    for i in range(len(l)):
        program.append(l[i])

    file.close()

    return program

def main():
    t = time.time()
    program = loadProgram(sys.argv[1])
    intr = Interpreter(program)

    initTime = time.time() - t

    intr.run()
    intr.printState()
    print('Init time   : ', initTime, 's')

if __name__ == '__main__':
    main()
