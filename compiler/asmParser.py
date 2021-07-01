import ASTparser as AST
from tokenizer import Token
import tokenNames as TN

# TODO: Pointers
# TODO: Int12 arithmetic
# TODO: BCD builtin
# TODO: Check redeclaration of variables

class VarInt:
    def __init__(self, name, addr):
        self.name = name
        self.addr = addr
        self.val  = None
        self.reg  = None

class VarSprite:
    def __init__(self, name, addr, len):
        self.name = name
        self.addr = addr
        self.val  = None
        self.len  = len

class VarPtr:
    def __init__(self, name, addr):
        self.name = name
        self.addr = addr
        self.val  = None
        self.reg  = None

class ConstPtr:
    def __init__(self, name, addr):
        self.name = name
        self.addr = addr

class UserFunction:
    def __init__(self, node, addr):
        self.name = node.name
        self.paramN = node.paramN
        self.addr = addr

class ASMParser:
    def __init__(self, asm, mode='file', verbose=True):
        self.MEM_OFF       = 0x200
        self.MEM_SIZE      = 4096 - self.MEM_OFF
        self.PROGRAM_START = 0x000 # 0
        self.RESVD_START   = self.MEM_SIZE-200
        self.STACK_START   = self.MEM_SIZE-100

        self.RUN_MODE       = mode
        self.MODE_FILE      = 'file'
        self.MODE_FUNC      = 'func'
        self.MODE_FUNCALLOC = 'funcAlloc'

        self.REG_ACC       = 0x0
        self.REG_F         = 0xF
        self.REG_CONST_1   = 0xE
        self.REG_STACK_LEN = 0xD

        self.BUILTIN_ARGN = {
            'clearScreen': 0, 'draw': 3, 'printN': 3, 'print1N': 3, 'print2N': 3,
            'Sprite': 2, '_BCD': 1, 'isKeyPressed': 1, '_getDelayTimer': 0,
            '_setSoundTimer': 1, '_setDelayTimer': 1, 'beep': 1, 'waitForKeypress': 0,
            'randInt': 1, 'spriteN': 1, 'sleep': 1
        }

        self.VERBOSE = verbose

        self.userFuncs = {}

        self.root = asm
        self.out  = []

        self.mem      = [ 0x00  for _ in range(self.MEM_SIZE) ]
        self.memAlloc = [ False for _ in range(self.MEM_SIZE) ]

        # V0: Operations and return
        # V1: Operations
        # V2 3 4 5 6 7 8 9: Variable
        # VA B C: Variable
        # VD: Stack length
        # VE: 1
        # VF: Flags
        self.reg  = [ 0x00 for _ in range(16) ]

        self.vars   = {}
        self.consts = {}
        self.varsStack = []

        self.programPtr = self.PROGRAM_START
        self.resvdPtr   = self.RESVD_START
        self.stackPtr    = self.STACK_START

    def isBuiltin(self, name):
        return name in TN.BUILTINS

    def raiseInvalidArgN(self, node, funcParamN):
        raise SyntaxError('Syntax error: ' + node.name + ' has ' + str(funcParamN)\
                        + ' arguments at line ' + str(node.line) + ' pos ' + str(node.pos))

    def emitProgram8(self, b8):
        if self.programPtr + 1 >= self.RESVD_START:
            raise MemoryError('Out of memory')
        else:
            self.mem[ self.programPtr ] = b8
            self.programPtr += 1

    def emitProgram16(self, b16):
        if self.programPtr + 2 >= self.RESVD_START:
            raise MemoryError('Out of memory')
        else:
            self.mem[ self.programPtr   ] = (b16 & 0xFF00) >> 8
            self.mem[ self.programPtr+1 ] = (b16 & 0x00FF)

            self.programPtr += 2

    def emitPushStack(self):
        self.emitSetI(self.stackPtr)
        self.emitAddRegToI(self.REG_STACK_LEN)
        self.emitAddNToReg(self.REG_STACK_LEN, 1)
        self.emitDump(0)

    def emitShiftStack(self):
        self.emitSubRegFromReg(self.REG_CONST_1, self.REG_STACK_LEN)
        self.emitSetI(self.stackPtr)
        self.emitAddRegToI(self.REG_STACK_LEN)
        self.emitLoad(0)

    def emitExit(self):
        self.emitProgram16(0x6001) # Set V0 to 1
        self.emitProgram16(0xBFFF) # Jump to 0xFFF + 1

    #
    # Memory management ----------------------------------------------------
    #

    # Return allocated memory address per the internal memory array
    def allocateMem(self, size):
        i = self.RESVD_START

        while i < self.MEM_SIZE:
            if not self.memAlloc[i]:
                # If none of i - i+size is allocated
                if all(map(lambda b: not b, self.memAlloc[i:i+size])):
                    for j in range(i, i+size):
                        self.memAlloc[j] = True

                    # TODO: Assuming linear memory allocation
                    self.resvdPtr = i+size
                    break

            i += 1

        if i == self.MEM_SIZE:
            raise MemoryError('Could not allocate space on memory')
        else:
            return i

    def setMem(self, addr, val):
        if val < 0x00 or val > 0xFF:
            # TODO: Add token info
            raise ValueError('setMem: Value to big to fit on a byte.')
        elif addr < 0 or addr >= self.MEM_SIZE:
            raise ValueError('setMem: Address out of bounds (' + hex(addr) + ').')
        else:
            self.mem[addr] = val


    #
    # Emits -------------------------------------------------------------------------------------------
    #

    def emitCls(self):                       self.emitProgram16(0x00E0)
    def emitReturnFromSubroutine(self):      self.emitProgram16(0x00EE)
    def emitJmp(self, addr):                 self.emitProgram16(0x1000 + addr + self.MEM_OFF)
    def emitCallSubroutine(self, addr):      self.emitProgram16(0x2000 + addr)
    def emitSkipIfRegEqN(self, r, n):        self.emitProgram16(0x3000 + 0x0100*r + n)
    def emitSkipIfRegNeqN(self, r, n):       self.emitProgram16(0x4000 + 0x0100*r + n)
    def emitSkipIfRegEqReg(self, r1, r2):    self.emitProgram16(0x5000 + 0x0100*r1 + 0x0010*r2)
    def emitSetRegToN(self, r, n):           self.emitProgram16(0x6000 + 0x0100*r + n)
    def emitAddNToReg(self, r, n):           self.emitProgram16(0x7000 + 0x0100*r + n)
    def emitSetRegToReg(self, r1, r2):       self.emitProgram16(0x8000 + 0x0100*r1 + 0x0010*r2)
    def emitOr(self, r1, r2):                self.emitProgram16(0x8001 + 0x0100*r1 + 0x0010*r2)
    def emitAnd(self, r1, r2):               self.emitProgram16(0x8002 + 0x0100*r1 + 0x0010*r2)
    def emitXor(self, r1, r2):               self.emitProgram16(0x8003 + 0x0100*r1 + 0x0010*r2)
    # def emitRShift
    # def emitLShift
    def emitAddRegToReg(self, vy, vx):       self.emitProgram16(0x8004 + 0x0100*vx + 0x0010*vy)
    def emitSubRegFromReg(self, vy, vx):     self.emitProgram16(0x8005 + 0x0100*vx + 0x0010*vy)
    def emitSetXtoYsubX(self, vx, vy):       self.emitProgram16(0x8006 + 0x0100*vx + 0x0010*vy)
    def emitSkipIfRegNeqReg(self, r1, r2):   self.emitProgram16(0x9000 + 0x0100*r1 + 0x0010*r2)
    def emitSetI(self, addr):                self.emitProgram16(0xA000 + addr + self.MEM_OFF)
    def emitJmpPlusV0(self, addr):           self.emitProgram16(0xB000 + addr)
    def emitRand(self, r):                   self.emitProgram16(0xC000 + 0x0100*r) # Mask is set by overwritting memory
    def emitDraw(self, x, y, n):             self.emitProgram16(0xD000 + 0x0100*x + 0x0010*y + n)
    def emitSkipIfRegKeyPressed(self, r):    self.emitProgram16(0xE09E + 0x0100*r)
    def emitSkipIfRegKeyNotPressed(self, r): self.emitProgram16(0xE0A1 + 0x0100*r)
    def emitSetRegToDelay(self, r):          self.emitProgram16(0xF007 + 0x0100*r)
    def emitWaitKeypress(self, r):           self.emitProgram16(0xF00A + 0x0100*r)
    def emitSetDelayToReg(self, r):          self.emitProgram16(0xF015 + 0x0100*r)
    def emitSetSoundToReg(self, r):          self.emitProgram16(0xF018 + 0x0100*r)
    def emitAddRegToI(self, r):              self.emitProgram16(0xF01E + 0x0100*r)
    def emitSetIToSpriteInReg(self, r):      self.emitProgram16(0xF029 + 0x0100*r)
    def emitBCD(self, r):                    self.emitProgram16(0xF033 + 0x0100*r)
    def emitDump(self, end):                 self.emitProgram16(0xF055 + 0x0100*end)
    def emitLoad(self, end):                 self.emitProgram16(0xF065 + 0x0100*end)


    #
    # ASM Conversion -----------------------------------------------------------------
    #
    
    def getVar(self, node):
        if node.name in self.vars:
            return self.vars[node.name]
        else:
            raise ValueError('Variable ' + node.name + ' doesnt exist (expand)')

    def storeRegInMem(self, r, addr):
        # Move reg to V0
        if r != self.REG_ACC:
            self.emitSetRegToReg(r, self.REG_ACC)

        # Set I
        self.emitSetI(addr)
        # Dump
        self.emitDump(0)

    def builtinDraw(self, node):
        argSprite, argX, argY = node.args
        rX, rY = 3, 4

        # Process x
        self.processExpr(argX.value, rX)
        # Process y
        self.processExpr(argY.value, rY)
        
        # var
        if type(argSprite.value) == AST.VarAST:
            var = self.getVar(argSprite.value)

            if type(var) == VarSprite:
                # Point to that memory
                self.emitSetI(var.addr)
                # Draw the sprite
                self.emitDraw(rX, rY, var.len)
            else:
                raise ValueError('builtinDraw: First argument to draw must be a sprite (expand)')
        # spriteN
        elif type(argSprite.value) == AST.FuncCallAST and argSprite.value.name == 'spriteN':
            self.processExpr(argSprite.value, 1) # TODO: !!!! Modifies I
            self.emitDraw(rX, rY, 5)
        else:
            raise ValueError('builtinDraw: Can\'t know sprite length (expand)')

        # Return if there was a colission
        self.emitSetRegToReg(self.REG_F, self.REG_ACC)

    def builtinPrintN(self, node, digitN):
        argN, argX, argY = node.args
        rX, rY = 5, 6 # 3, 4 used by draw
        spriteLen = 5

        # Process x
        self.processExpr(argX.value, rX)
        # Process y
        self.processExpr(argY.value, rY)
        # Process N (last so that the value remains in V0)
        self.processExpr(argN.value, self.REG_ACC)

        addr = self.allocateMem(3)
        self.emitSetI(addr) # Point address to the store address
        self.emitBCD(self.REG_ACC)

        if digitN == 3:
            self.emitLoad(0)
            self.emitSetIToSpriteInReg(self.REG_ACC) # Point to first digit
            self.emitDraw(rX, rY, spriteLen) # Print the first digit

        if digitN >= 2:
            self.emitSetI(addr+1)
            self.emitLoad(0)
            self.emitSetIToSpriteInReg(self.REG_ACC) # Point to second digit

            self.emitAddNToReg(rX, spriteLen) # x += 3
            self.emitDraw(rX, rY, spriteLen)

        if digitN >= 1:
            self.emitSetI(addr+2)
            self.emitLoad(0)
            self.emitSetIToSpriteInReg(self.REG_ACC) # Point to third digit (reuse V0)

            self.emitAddNToReg(rX, spriteLen) # x += 5
            self.emitDraw(rX, rY, spriteLen)

    def builtinBCD(self, node):
        addr = self.allocateMem(3) # Allocate 3 bytes for the nums

        # Declare ptr constant
        var = ConstPtr(node.name, addr)
        self.consts[var.name] = var

        # Process argument expression
        self.processExpr(node.args[0], self.REG_ACC)

        # Point I to addr and emit BCD from the expression
        self.emitSetI(addr)
        self.emitBCD(self.REG_ACC)

    def builtinSpriteN(self, node):
        self.processExpr(node.args[0].value, self.REG_ACC)
        self.emitSetIToSpriteInReg(self.REG_ACC)

    def builtinRandInt(self, node):
        self.processExpr(node.args[0].value, self.REG_ACC)

        # Modify mask 5 positions ahead (2 of set I, 2 of dump and 1 of rand)
        self.emitSetI(self.programPtr+5)
        self.emitDump(0)

        self.emitRand(self.REG_ACC)

    def builtinSleep(self, node):
        self.processExpr(node.args[0].value, self.REG_ACC)
        self.emitSetDelayToReg(self.REG_ACC)

        # loop
        self.emitSetRegToDelay(self.REG_ACC)   # <-+
        self.emitSkipIfRegEqN(self.REG_ACC, 0) #   |
        self.emitJmp(self.programPtr-4)        # --+


    #
    # Expr
    #

    # Process operator and leave result in reg r
    def processExpr(self, op, r):
        # Sorted by priority
        if type(op) == AST.Int8AST:
            self.emitSetRegToN(r, op.value)
        elif type(op) == AST.Int12AST:
            # ptrs are constant expressions so they will be stored in V0, V1
            self.emitSetRegToN(r,   op.value >> 8)    # 0xF00
            self.emitSetRegToN(r+1, op.value & 0x0FF) # 0x0FF
        elif type(op) == AST.VarAST:
            if op.name not in self.vars:
                raise ValueError('processExpr: Variable ' + op.name + ' not found (expand)')
            else:
                # TODO: Optimizations
                self.emitSetI(self.vars[op.name].addr) # Point I to addr
                self.emitLoad(0) # Load var into v0
                self.emitSetRegToReg(r, 0x0) # Move V0 to r
        elif type(op) == AST.FuncCallAST:
            self.funcCall(op) # Assuming V0
            self.emitSetRegToReg(r, 0x0) # Move V0 to r
        elif type(op) == AST.OpAST:
            # Unary operators
            if op.type == 'deref':
                # Set I to addr
                # Copy 0xIF00 and 0x00FF values to regs
                ...
            elif op.type in 'neg':
                ...
            # Binary operators
            elif op.type in ['bitOr', 'bitAnd', 'bitXor', 'sum', 'sub', 'mult',
                             'lt', 'gt', 'let', 'get', 'eq', 'neq']:
                self.processExpr(op.left,  0) # leftexpr -> V0
                self.emitPushStack()          # V0 -> stack
                self.processExpr(op.right, 1) # rightexpr -> V1
                self.emitShiftStack()         # stack -> V0

                if op.type in ['bitOr', 'bitAnd', 'bitXor', 'sum', 'sub', 'mult']:
                    if   op.type == 'bitOr'  : self.emitOr(0, 1)
                    elif op.type == 'bitAnd' : self.emitAnd(0, 1)
                    elif op.type == 'bitXor' : self.emitXor(0, 1)
                    elif op.type == 'sum'    : self.emitAddRegToReg(1, 0)
                    elif op.type == 'sub'    : self.emitSubRegFromReg(1, 0)
                    elif op.type == 'mult'   : raise ValueError('processExpr: mult not implemented')

                    if r != 0x0: self.emitSetRegToReg(r, 0x0) # Set r to 0x0
                elif op.type in ['lt', 'gt', 'let', 'get']:
                    if op.type == 'lt':
                        # Just move VF^1 to r (VF is set to 1 when there is *not* a borrow)
                        self.emitSubRegFromReg(0x1, 0x0)
                        self.emitSetRegToReg(r, 0xF)
                        self.emitXor(r, self.REG_CONST_1)
                    elif op.type == 'gt':
                        self.emitSubRegFromReg(0x0, 0x1)
                        self.emitSetRegToReg(r, 0xF)
                        self.emitXor(r, self.REG_CONST_1)
                    elif op.type == 'let': # not gt
                        self.emitSubRegFromReg(0x0, 0x1)
                        self.emitSetRegToReg(r, 0xF)
                    elif op.type == 'get': # not lt
                        self.emitSubRegFromReg(0x1, 0x0)
                        self.emitSetRegToReg(r, 0xF)
                elif op.type in ['eq', 'neq']:
                    if op.type == 'eq':
                        # v2 = 0; if(v0 == v1): v2 = 1
                        self.emitSetRegToN(0x2, 0)
                        self.emitSkipIfRegNeqReg(0, 1)
                        self.emitSetRegToN(0x2, 1)
                    elif op.type == 'neq':
                        # v2 = 0; if(v0 != v1): v2 = 1
                        self.emitSetRegToN(0x2, 0)
                        self.emitSkipIfRegEqReg(0, 1)
                        self.emitSetRegToN(0x2, 1)

                    # Copy V2 to r
                    if r != 0x02: self.emitSetRegToReg(r, 0x2)
            elif op.type in ['logAnd', 'logOr']:
                self.processExpr(op.left, r) # leftexpr -> V0

                # Evaluate left side
                # If left side is 1, leave it in r and jump to the end
                # If its 0, evaluate right side in r
                if op.type == 'logAnd':
                    # If left side is not zero, evaluate right side
                    self.emitSkipIfRegNeqN(0x0, 0)
                elif op.type == 'logOr':
                    # If left side is zero, evaluate right side
                    self.emitSkipIfRegEqN(0x0, 0)

                # Save two bytes for the jump if the left side evaluates to false
                jmpAddr = self.programPtr
                self.programPtr += 2

                # Emit right side and leave the value of it in r
                self.processExpr(op.right, r) # rightexpr -> r

                # Jump from jmpAddr to here
                self.setMem(jmpAddr,   (0x1000 + self.programPtr + self.MEM_OFF) >> 8)
                self.setMem(jmpAddr+1, (0x1000 + self.programPtr + self.MEM_OFF) & 0x00FF)

                # Move acc to r
                if r != self.REG_ACC: self.emitSetRegToReg(r, self.REG_ACC)
            else:
                raise ValueError('processExpr: Incorrect operand type')
        else:
            print('op', op)
            raise ValueError('processExpr: Incorrect operand class type')


    #
    # Variable and constant declaration --------------------------------------
    #

    def declareInt(self, node):
        # Allocate memory and register as variable
        addr = self.allocateMem(1)
        newVar = VarInt(node.name, addr)

        self.vars[node.name] = newVar

        # If its a constant numeric value, store it in resvd
        if type(node.expr) == AST.Int8AST:
            newVar.val = node.expr.value
            self.setMem(newVar.addr, newVar.val)
        elif type(node.expr) == AST.Int12AST:
            raise ValueError('declareInt: Value too big for an int (expand)')
        else:
            # Process the expression
            self.processExpr(node.expr, self.REG_ACC)
            # Store the reg in mem
            self.storeRegInMem(self.REG_ACC, addr)

    def declareSprite(self, node):
        addr   = self.allocateMem(node.len)
        newVar = VarSprite(node.name, addr, node.len)

        self.vars[node.name] = newVar

        # Store the sprite in memory
        for i in range(node.len):
            self.setMem(addr+i, node.values[i])

    def declarePtr(self, node):
        addr   = self.allocateMem(2)
        newVar = VarPtr(node.name, addr)

        self.vars[node.name] = newVar

        # If its a constant numeric value, store it in resvd
        if type(node.expr) in [AST.Int8AST, AST.Int12AST]:
            newVar.val = node.expr.value

            # Store in memory adding 0xA000 to set I
            self.setMem(newVar.addr,   (0xA000 + newVar.val + self.MEM_OFF) >> 8)    # 0xF00
            self.setMem(newVar.addr+1, (0xA000 + newVar.val + self.MEM_OFF) & 0x00FF) # 0x0FF
        else:
            r = 0

            # Process the expression
            self.processExpr(node.expr, r)
            # Store the reg in mem (add 0xA0 to modify I)
            self.emitAddNToReg(0xA0, r)
            self.storeRegInMem(r,   addr)
            self.storeRegInMem(r+1, addr+1)

    def assignVar(self, node):
        if node.name not in self.vars:
            raise 'Variable ' + node.name + ' doesn\'t exist (expand)'
        else:
            r = 0
            var = self.vars[node.name]
            token = Token(var.name, node.line, node.pos)

            if node.type == 'assign':
                self.processExpr(node.value, r)
            elif node.type == 'addAssign':
                self.processExpr(AST.OpAST('sum', AST.VarAST(token), node.value), r)
            elif node.type == 'subAssign':
                self.processExpr(AST.OpAST('sub', AST.VarAST(token), node.value), r)
            elif node.type == 'andAssign':
                self.processExpr(AST.OpAST('and', AST.VarAST(token), node.value), r)
            elif node.type == 'orAssign':
                self.processExpr(AST.OpAST('or',  AST.VarAST(token), node.value), r)
            elif node.type == 'xorAssign':
                self.processExpr(AST.OpAST('xor', AST.VarAST(token), node.value), r)
            else:
                raise ValueError('Invalid assign type: ' + node.type + ' (expand)')

            self.storeRegInMem(r, var.addr)

    #
    # Functions --------------------------------------------------------------
    #

    # Allocate memory for a user defined function
    # TODO: Check for redeclaration
    def funcDecl(self, node):
        # Run the body to get the number of bytes it would need
        funcStartAddr = self.programPtr

        # Emit the function code
        self.RUN_MODE = self.MODE_FUNC
        self.processLeaf(node.left) # TODO: Move this to runExec
        self.runExec(node.right)    #
        self.RUN_MODE = self.MODE_FUNCALLOC

        # Emit the return
        self.emitReturnFromSubroutine()

        # Allocate and store function
        func = UserFunction(node, funcStartAddr+self.MEM_OFF)
        self.userFuncs[func.name] = func

    def funcCallBuiltin(self, node):
        # Check argument number
        if node.argN != self.BUILTIN_ARGN[node.name]:
            self.raiseInvalidArgN(node, self.BUILTIN_ARGN[node.name])

        if node.name == 'clearScreen':
            self.emitCls()
        elif node.name == 'waitForKeypress':
            r = self.REG_ACC
            self.emitWaitKeypress(r) # Leave return in r
        elif node.name == 'draw':
            self.builtinDraw(node)
        elif node.name == 'printN':
            self.builtinPrintN(node, 3)
        elif node.name == 'print1N':
            self.builtinPrintN(node, 1)
        elif node.name == 'print2N':
            self.builtinPrintN(node, 2)
        elif node.name == '_BCD':
            #r = self.REG_ACC
            self.builtinBCD(node)
        elif node.name == 'isKeyPressed':
            # if(key is pressed) { r = 1 }
            r = self.REG_ACC
            self.emitSetRegToN(r, 0)
            self.emitSkipIfRegKeyNotPressed(r)
            self.emitSetRegToN(r, 1)
        elif node.name == '_getDelayTimer':
            r = self.REG_ACC
            self.emitSetRegToDelay(r)
        elif node.name == '_setDelayTimer':
            r = self.REG_ACC
            self.processExpr(node.args[0].value, r)
            self.emitSetDelayToReg(r)
        elif node.name == '_setSoundTimer':
            r = self.REG_ACC
            self.processExpr(node.args[0].value, r)
            self.emitSetSoundToReg(r)
        elif node.name == 'beep':
            r = self.REG_ACC
            self.processExpr(node.args[0].value, r)
            self.emitSetSoundToReg(r)
        elif node.name == 'randInt':
            self.builtinRandInt(node)
        elif node.name == 'spriteN':
            self.builtinSpriteN(node)
        elif node.name == 'sleep':
            self.builtinSleep(node)
        else:
            raise ValueError('funcCallBuiltin: Incorrect builtin name: ' + node.name)

    def funcCallUser(self, node):
        if node.name not in self.userFuncs:
            raise ValueError('Function ' + node.name + ' not defined.')
        else:
            func = self.userFuncs[node.name]
            self.emitCallSubroutine(func.addr)

    # Function call
    def funcCall(self, node):
        if node is not None:
            if self.isBuiltin(node.name):
                self.funcCallBuiltin(node)
            else:
                self.funcCallUser(node)


    #
    # Blocks
    #


    #               if cond
    #               -------
    #               ------- >-----------------+
    #     +-------< end if                    |
    #     |         else if cond              |
    #     |         -------                   |
    #     |         ------- >-----------------+
    #     |-------< end else if               |
    #     |         ...     >-----------------|
    #     |         if 1 (else)               |
    #     |         -------                   |
    #     +-------> end else <----------------+
    def ifBlock(self, node):
        r = 0

        self.processExpr(node.cond, r)

        # 1. Skip jump if r != 0
        self.emitSkipIfRegNeqN(r, 0)

        # 1. Reserve space for the jump to the next if
        blockEndJmp = self.programPtr
        self.programPtr += 2

        # 2. Emit if block
        self.runExec(node.left)

        # 1. Block end
        # Offset of 2 to account for the jump instruction
        self.setMem(blockEndJmp,   (0x1000 + self.MEM_OFF + self.programPtr + 2) >> 8)
        self.setMem(blockEndJmp+1, (0x1000 + self.MEM_OFF + self.programPtr + 2) & 0x00FF)

        #if not node.isLast:
        # 3. Reserve space to jump to the end of all ifs
        endJmpAddr = self.programPtr
        self.programPtr += 2

        # 4. Go to the next if
        self.processLeaf(node.right)

        # 3. If - else if - else end
        self.setMem(endJmpAddr,   (0x1000 + self.MEM_OFF + self.programPtr) >> 8)
        self.setMem(endJmpAddr+1, (0x1000 + self.MEM_OFF + self.programPtr) & 0x00FF)

    def whileBlock(self, node):
        # Reserve 2 bytes for the start addr
        r = 0

        whileStart = self.programPtr

        self.processExpr(node.cond, r)
        self.emitSkipIfRegNeqN(r, 0)

        # Reserve 2 bytes to jump to the end of the while if its false
        whileSkipJmp = self.programPtr
        self.programPtr += 2

        self.runExec(node)

        # Jump back to the start of the block
        self.setMem(self.programPtr,   (0x1000 + self.MEM_OFF + whileStart) >> 8)
        self.setMem(self.programPtr+1, (0x1000 + self.MEM_OFF + whileStart) & 0x00FF)
        self.programPtr += 2


        # Jump here if the condition evaluates to false
        self.setMem(whileSkipJmp,   (0x1000 + self.MEM_OFF + self.programPtr) >> 8)
        self.setMem(whileSkipJmp+1, (0x1000 + self.MEM_OFF + self.programPtr) & 0x00FF)


    #
    # Tree processing ------------------------------------------------------------
    #

    # Process leaf nodes
    def processLeaf(self, node):
        if node is not None:
            if self.RUN_MODE != self.MODE_FUNCALLOC:
                if self.VERBOSE: print('leaf', node)

                if type(node) == AST.IntDeclAST:
                    self.declareInt(node)
                elif type(node) == AST.SpriteDeclAST:
                    self.declareSprite(node)
                elif type(node) == AST.PtrDeclAST:
                    self.declarePtr(node)
                elif type(node) == AST.VarAssignAST:
                    self.assignVar(node)
                elif type(node) == AST.FuncDeclAST:
                    # TODO: They can be defined inside if and while blocks
                    if self.RUN_MODE != self.MODE_FILE:
                        raise SyntaxError('Functions can only be defined on topmost level (expand)')
                elif type(node) == AST.FuncCallAST:
                    self.funcCall(node)
                elif type(node) == AST.IfAST:
                    self.ifBlock(node)
                elif type(node) == AST.WhileAST:
                    self.whileBlock(node)
                elif type(node) == AST.ReturnAST:
                    if self.RUN_MODE != self.MODE_FUNC:
                        raise SyntaxError('Return can only be used from subroutines (expand)')
                    else:
                        # Leave return value in V0
                        r = self.REG_ACC
                        self.processExpr(node.expr, r)
            else:
                if type(node) == AST.FuncDeclAST:
                    self.funcDecl(node)

    # Process branches
    def runExec(self, node=None):
        # If root is not None
        if node is not None:
            if self.RUN_MODE != self.MODE_FUNCALLOC:
                if type(node) in [AST.ExecAST, AST.WhileAST]:
                    self.processLeaf(node.left)
                    self.runExec(node.right)
            else: # In mode func alloc only allow functions in ExecAST nodes
                if type(node) == AST.ExecAST:
                    self.processLeaf(node.left)
                    self.runExec(node.right)

    def _run(self):
        # Set VE to 1 as a constant
        if self.RUN_MODE == self.MODE_FILE:
            self.emitSetRegToN(self.REG_CONST_1, 1)

        # Save 2 bytes for the jump to the end of function declarations
        jmpToProgramStart = self.programPtr
        self.programPtr += 2

        if self.RUN_MODE == self.MODE_FILE:
            self.RUN_MODE = self.MODE_FUNCALLOC
            self.runExec(self.root)
            self.RUN_MODE = self.MODE_FILE

        self.setMem(jmpToProgramStart,   (0x1000 + self.programPtr + self.MEM_OFF) >> 8)
        self.setMem(jmpToProgramStart+1, (0x1000 + self.programPtr + self.MEM_OFF) & 0x00FF)

        self.runExec(self.root)

        if self.RUN_MODE == self.MODE_FILE:
            self.emitExit()


    #----------------------------------------------------------------------------

    def printMemSeg(self, fromAddr, toAddr):
        print(' 00: ', end='')
        i = 0
        for n in self.mem[fromAddr:toAddr+1]:
            print(hex(n)[2:].zfill(2), end=' ')
            if i%40 == 0 and i != 0: print('\n' + str(i).zfill(3) + ': ', end='')
            i += 1

        print('\n')

    def emitCode(self, path):
        #('MEM')
        #self.printMemSeg(0x000, 0xFFF)

        if self.VERBOSE:
            print('\nPROGRAM')
            self.printMemSeg(0, self.programPtr-1) # Careful
            print('RESVD')
            self.printMemSeg(self.RESVD_START, self.resvdPtr-1) # Careful
            print()

        file = open(path, 'wb')
        file.write(bytes(self.mem))
        file.close()
