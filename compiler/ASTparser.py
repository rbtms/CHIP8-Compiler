import re
import tokenNames as TN
from tokenizer import Token

# Helper function
def printAST(ast, padding):
    if ast is not None:
        if type(ast) == ExecAST:
            printAST(ast.left, padding)
            printAST(ast.right, padding)
        elif type(ast) in [FuncDeclAST, WhileAST]:
            print(' '*padding, ast)
            printAST(ast.left, padding+4)
            printAST(ast.right, padding+4)
        elif type(ast) == IfAST:
            print(' '*padding, ast)
            printAST(ast.left, padding+4)
            printAST(ast.right, padding)
        else:
            print(' '*padding, ast)

# Left is the current operation
# Right is the next ExecAST tree
class ExecAST:
    def __init__(self, left, right):
        self.ASTType = 'exec'
        self.left = left
        self.right = right

class FuncParamAST:
    def __init__(self, type, token):
        self.ASTType = 'funcParam'
        self.type = type
        self.name = token.token
        self.line = token.line
        self.pos  = token.pos

    def __str__(self):
        return 'FuncParam(' + self.type + ' ' + self.name + ')'

class FuncArgAST:
    def __init__(self, firstToken, expr):
        self.ASTType = 'funcArg'
        self.line = firstToken.line
        self.pos  = firstToken.pos
        self.value = expr

    def __str__(self):
        return 'FuncArg '+str(self.value)

class FuncDeclAST:
    def __init__(self, type, token, params, bodyExecAST):
        self.ASTType = 'func'
        self.type   = type
        self.name   = token.token
        self.line   = token.line
        self.pos    = token.pos
        self.paramN = len(params)
        self.params = params
        self.left   = bodyExecAST.left
        self.right  = bodyExecAST.right

    def __str__(self):
        return 'FuncDecl ' + self.type + ' ' + self.name\
               + ' ' + str(list(map(str, self.params)))

class FuncCallAST:
    def __init__(self, token, args):
        # TODO: Do this somewhere else
        args = list(filter(lambda a: a.value is not None, args))

        self.ASTType = 'funcCall'
        self.name = token.token
        self.line = token.line
        self.pos  = token.pos
        self.argN = len(args)
        self.args = args

    def __str__(self):
        return 'FuncCall '+self.name + ' ' + str(self.argN)\
               + ' <' + str(list(filter(bool, map(str, self.args)))) + '>'

class VarAST:
    def __init__(self, token):
        self.ASTType = 'var'
        self.name = token.token
        self.line = token.line
        self.pos  = token.pos

    def __str__(self):
        return 'Var(' + self.name + ')'

class IntDeclAST:
    def __init__(self, token, expr):
        self.ASTType = 'intDecl'
        self.name    = token.token
        self.line    = token.line
        self.pos     = token.pos
        self.expr    = expr

    def __str__(self):
        return 'IntDecl ' + self.name + ' [' + str(self.expr) + ']'

class SpriteDeclAST:
    def __init__(self, token, values):
        self.ASTType = 'spriteDecl'
        self.name    = token.token
        self.line    = token.line
        self.pos     = token.pos
        self.values  = values
        self.len     = len(values)

    def __str__(self):
        return 'SpriteDecl ' + self.name + ' <' + str(self.values) + '>'

class PtrDeclAST:
    def __init__(self, token, expr):
        self.ASTType = 'ptrDecl'
        self.name    = token.token
        self.line    = token.line
        self.pos     = token.pos
        self.expr    = expr

    def __str__(self):
        return 'PtrDeclAST ' + self.name + ' ' + str(self.expr)

class VarAssignAST:
    def __init__(self, token, assignType, value):
        self.ASTType = 'varAssign'
        self.type    = assignType
        self.name    = token.token
        self.line    = token.line
        self.pos     = token.pos
        self.value   = value

    def __str__(self):
        return 'VarAssign ' + self.type + ' ' + self.name + ' [' + str(self.value) + ']'

# Else is added as an else if with a condition evaluating to True
class IfAST:
    def __init__(self, exprAST, execAst):
        self.ASTType = 'if'
        self.cond  = exprAST
        self.left  = execAst
        self.right = None

    def __str__(self):
        return 'if ' + str(self.cond)

class WhileAST:
    def __init__(self, expr, ast):
        self.ASTType = 'while'
        self.cond = expr
        self.left = ast.left
        self.right = ast.right

    def __str__(self):
        return 'While ' + str(self.cond)

class ReturnAST:
    def __init__(self, expr):
        self.ASTType = 'return'
        self.expr = expr

    def __str__(self):
        return 'Return ' + str(self.expr)

class Int8AST:
    def __init__(self, token, num):
        self.ASTType = 'num'
        self.line = token.line
        self.pos = token.pos
        self.value = num

    def __str__(self):
        return 'Int8('+str(self.value)+')'

class Int12AST:
    def __init__(self, token, num):
        self.ASTType = 'num'
        self.line = token.line
        self.pos = token.pos
        self.value = num

    def __str__(self):
        return 'Int12('+str(self.value)+')'

# TODO: Add location
class OpAST:
    def __init__(self, opType, left, right):
        self.ASTType = 'op'
        self.type = opType
        self.left = left
        self.right = right

    def __str__(self):
        return 'OpAST('+self.type + ' ' + str(self.left) + ' ' + str(self.right) + ')'


class ASTParser:
    def __init__(self, tokens):
        self.EXPR_ASSIGN   = 'assign'
        self.EXPR_FIRSTARG = 'firstArg'
        self.EXPR_LASTARG  = 'lastArg'

        self.REGEX_RESERVED = re.compile('|'.join(TN.RESERVED))

        self.REGEX_IDENT = re.compile('[_a-zA-Z][\w\d]*')
        self.REGEX_DEC = re.compile('^\d+$')
        self.REGEX_HEX = re.compile('^0x[0123456789ABCDEFabcdef]+$')
        self.REGEX_BIN = re.compile('^b[01]+$')

        self.SEMICOLON_TOKEN = Token(';', -1, -1)

        self.tokens = tokens
        self.asts = []

        self.tokenPos = 0

    def isIdentifier(self, token=None):
        if token is None: token = self.getToken()
        return self.REGEX_IDENT.match(token.token) is not None

    def isReserved(self, token=None):
        if token is None: token = self.getToken()
        return self.REGEX_RESERVED.match(token.token) is not None

    def isDec(self, token=None):
        if token is None: token = self.getToken()
        return self.REGEX_DEC.match(token.token) is not None

    def isHex(self, token=None):
        if token is None: token = self.getToken()
        return self.REGEX_HEX.match(token.token) is not None

    def isBin(self, token=None):
        if token is None: token = self.getToken()
        return self.REGEX_BIN.match(token.token) is not None

    def isNum(self, token=None):
        if token is None: token = self.getToken()
        return self.isDec(token) or self.isHex(token) or self.isBin(token)

    def isInt8(self, token=None):
        if token is None: token = self.getToken()
        return self.isNum(token) and self.parseNum(token.token) < 0x100

    def isInt12(self, token=None):
        if token is None: token = self.getToken()
        return self.isNum(token) and self.parseNum(token.token) > 0xFF\
            and self.parseNum(token.token) < 0x1000

    def parseNum(self, num):
        if num[0] == 'b':
            return int(num[1:], 2)
        elif num[0:2] == '0x':
            return int(num[2:], 16)
        else:
            return int(num)

    def getToken(self):
        if self.tokenPos < len(self.tokens):
            return self.tokens[self.tokenPos]
        else:
            return None

    def peekToken(self, index=1):
        if self.tokenPos+index < len(self.tokens):
            return self.tokens[self.tokenPos+index]
        else:
            raise IndexError('peekToken: Index out of bounds.')

    def prevToken(self):
        self.tokenPos -= 1
        return self.getToken()

    def nextToken(self):
        self.tokenPos += 1
        return self.getToken()

    def raiseInvalidToken(self, token):
        raise ValueError('Invalid token ' + token.token
                         + ' at line ' + str(token.line) + ' pos ' + str(token.pos))

    def raiseUnmatchedParens(self, token):
        raise ValueError('Un matched parens at line ' + token.line + ' pos ' + token.pos)


    #
    # Find
    #

    # Return expression until it finds a ; or a ,
    # Expression lookup
    def findExpr(self, exprType):
        parens = 0
        expr = []

        while True:
            # First argument
            if self.getToken().token == ',':
                if parens != 0:
                    if exprType != self.EXPR_ASSIGN:
                        self.raiseUnmatchedParens(self.getToken())
                elif exprType == self.EXPR_FIRSTARG:
                    break
                else:
                    self.raiseInvalidToken(self.getToken())
            # Declaration / assignation
            elif self.getToken().token == ';':
                if parens != 0:
                    self.raiseUnmatchedParens(self.getToken())
                elif exprType == self.EXPR_ASSIGN:
                    break
                else:
                    self.raiseInvalidToken(self.getToken())
            elif self.getToken().token == '(':
                parens -= 1
            # Last argument
            elif self.getToken().token == ')':
                if exprType == self.EXPR_LASTARG and parens == 0:
                    break
                elif parens >= 0:
                    self.raiseUnmatchedParens(self.getToken())
                else:
                    parens += 1

            expr.append(self.getToken())
            self.nextToken()

        self.nextToken()

        return expr

    # TODO: Implement FIRST_ARG and LAST_ARG
    # Sprite declaration value lookup
    def findSprite(self, exprType):
        sprite = []

        while True:
            if self.isInt8():
                sprite.append( self.parseNum(self.getToken().token) )
            elif self.getToken().token == ';':
                break
            else:
                self.raiseInvalidToken(self.getToken())

            self.nextToken()

        self.nextToken() # (; , )) -> next

        return sprite

    def findNextArgParam(self):
        parens = 0
        tokens = []

        while True:
            if self.getToken().token == ';':
                break
            elif self.getToken().token == ',':
                if parens == 0:
                    break
            elif self.getToken().token == ')':
                parens += 1

                if parens == 1:
                    break
            elif self.getToken().token == '(':
                parens -= 1

            tokens.append(self.getToken())
            self.nextToken()

        return tokens


    # Function argument lookup
    def findFuncParams(self):
        params = []

        while True:
            self.nextToken() # [( paramName ,] -> [paramType , )]
            token = self.getToken()

            if token.token in ('int', 'sprite'):
                self.nextToken() # type -> identifier

                if self.isIdentifier():
                    params.append( FuncParamAST(token.token, self.getToken()) )
                else:
                    self.raiseInvalidToken(self.getToken())
            elif token.token == ')':
                self.nextToken() # ) -> {
                break
            elif token.token != ',':
                self.raiseInvalidToken(token)

        return params

    def findFuncArgs(self):
        parens = 0
        args = []
        arg = []

        while True:
            self.nextToken() # [( ,] -> [args,)]

            if self.getToken().token == ',':
                if parens == 0:
                    # func(,expr,expr)
                    if not arg:
                        self.raiseInvalidToken(self.getToken())
                    else:
                        args.append( FuncArgAST(arg[0], self.expr(arg)) )
                        arg = []
                else:
                    arg.append(self.getToken())
            else:
                if self.getToken().token == '(':
                    parens -= 1
                elif self.getToken().token == ')':
                    parens += 1

                    if parens == 1:
                        # TODO: Test , and )
                        if self.peekToken(1).token in ';,)':
                            # Add last arg
                            # Commented to avoid including ) in the expr
                            #arg.append(self.getToken())

                            if arg:
                                args.append( FuncArgAST(arg[0], self.expr(arg)) )

                            self.nextToken()
                            self.nextToken()

                            break
                        else:
                            self.raiseInvalidToken(self.peekToken(1))

                arg.append(self.getToken())

        return args

    # Body tokens lookup
    def findBody(self):
        brackets = 1
        tokens = []

        self.nextToken() # { -> body

        while True:
            if self.getToken().token == '}':
                brackets -= 1

                if brackets == 0:
                    self.nextToken()
                    break
            elif self.getToken().token == '{':
                brackets += 1

            tokens.append(self.getToken())
            self.nextToken() # others -> [others }]

        return tokens


    #
    # Expression parsing
    #

    def isFuncName(self, tokens, i=0):
        if i+1 >= len(tokens):
            return False
        else:
            return self.REGEX_IDENT.match(tokens[i].token) is not None\
                and tokens[i+1].token == '('

    # Precedence:
    #   1: == != <= >= < >
    #   2: + -
    #   3: *
    #   4: & | ^
    #   5: !
    def expr(self, tokens):
        i = 0
        parens = 0
        #print(list(map(lambda t: t.token, tokens)))

        if not tokens:
            return None

        # Remove parents if its surounded by them
        if parens == 0 and tokens[0].token == '(' and tokens[-1].token == ')':
            isSurounded = True

            for t in tokens[1:-1]:
                if t.token in '()':
                    if t.token == ')':
                        isSurounded = False

                    break

            for t in list(reversed(tokens))[1:-1]:
                if t.token in '()':
                    if t.token == '(':
                        isSurounded = False

            if isSurounded:
                tokens = tokens[1:-1]

        i, parens = 0, 0
        # Check for logical operators
        while i < len(tokens):
            if   tokens[i].token == '(': parens -= 1
            elif tokens[i].token == ')': parens += 1

            if parens == 0:
                if tokens[i].token == '&&':
                    return OpAST('logAnd', self.expr(tokens[:i]), self.expr(tokens[i+1:]))
                elif tokens[i].token == '||':
                    return OpAST('logOr', self.expr(tokens[:i]), self.expr(tokens[i+1:]))

            i += 1

        # Check for equalities
        i, parens = 0, 0
        while i < len(tokens):
            if   tokens[i].token == '(': parens -= 1
            elif tokens[i].token == ')': parens += 1

            if parens == 0:
                if tokens[i].token == '==':
                    return OpAST('eq', self.expr(tokens[:i]), self.expr(tokens[i+1:]))
                elif tokens[i].token == '!=':
                    return OpAST('neq', self.expr(tokens[:i]), self.expr(tokens[i+1:]))
                elif tokens[i].token == '<=':
                    return OpAST('let', self.expr(tokens[:i]), self.expr(tokens[i+1:]))
                elif tokens[i].token == '>=':
                    return OpAST('get', self.expr(tokens[:i]), self.expr(tokens[i+1:]))
                elif tokens[i].token == '<':
                    return OpAST('lt', self.expr(tokens[:i]), self.expr(tokens[i+1:]))
                elif tokens[i].token == '>':
                    return OpAST('gt', self.expr(tokens[:i]), self.expr(tokens[i+1:]))

            i += 1

        i, parens = 0, 0
        # Check for + -
        while i < len(tokens):
            if   tokens[i].token == '(': parens -= 1
            elif tokens[i].token == ')': parens += 1

            if parens == 0:
                if tokens[i].token == '+':
                    return OpAST('sum', self.expr(tokens[:i]), self.expr(tokens[i+1:]))
                elif tokens[i].token == '-':
                    return OpAST('sub', self.expr(tokens[:i]), self.expr(tokens[i+1:]))

            i += 1

        # Check for *
        i, parens = 0, 0
        while i < len(tokens):
            if   tokens[i].token == '(': parens -= 1
            elif tokens[i].token == ')': parens += 1

            if parens == 0:
                if tokens[i].token == '*':
                    return OpAST('mult', self.expr(tokens[:i]), self.expr(tokens[i+1:]))

            i += 1

        # Check for bitwise operators
        i, parens = 0, 0
        while i < len(tokens):
            if   tokens[i].token == '(': parens -= 1
            elif tokens[i].token == ')': parens += 1

            if parens == 0:
                if tokens[i].token == '&':
                    return OpAST('bitAnd', self.expr(tokens[:i]), self.expr(tokens[i+1:]))
                elif tokens[i].token == '|':
                    return OpAST('bitOr', self.expr(tokens[:i]), self.expr(tokens[i+1:]))
                elif tokens[i].token == '^':
                    return OpAST('bitXor', self.expr(tokens[:i]), self.expr(tokens[i+1:]))

            i += 1

        # It has no operators thus it must be evaluable
        if self.isInt8(tokens[0]):
            return Int8AST(tokens[0], self.parseNum(tokens[0].token))
        elif self.isInt12(tokens[0]):
            return Int12AST(tokens[0], self.parseNum(tokens[0].token))
        elif self.isNum(tokens[0]):
            raise ValueError('expr: Number too large to be int8 or int12 (expand)')
        elif self.isFuncName(tokens):
            # Return only the first element of the AST
            return ASTParser(tokens + [self.SEMICOLON_TOKEN]).run().left
        elif tokens[0].token in '!@#':
            if len(tokens) > 1:
                if   tokens[0].token == '!': return OpAST('neg',   self.expr(tokens[1:]), None)
                elif tokens[0].token == '@': return OpAST('deref', self.expr(tokens[1:]), None)
                elif tokens[0].token == '#': return OpAST('ref',   self.expr(tokens[1:]), None)
            else:
                self.raiseInvalidToken(tokens[0])
        elif self.isIdentifier(tokens[0]):
            return VarAST(tokens[0])
        else:
            return None

    #
    # Declarations
    #

    def varDeclaration(self, varType, nameToken):
        endToken = self.getToken()
        self.nextToken() # = -> expr

        if varType == 'int':
            if endToken.token == '=':
                exprTokens = self.findExpr(self.EXPR_ASSIGN)
                return IntDeclAST(nameToken, self.expr(exprTokens))
            elif endToken.token == ';':
                # Initialize a 0 with the position of endToken
                return IntDeclAST(nameToken, Int8AST(endToken, 0))
        elif varType == 'sprite':
            sprite = self.findSprite(self.EXPR_ASSIGN)
            return SpriteDeclAST(nameToken, sprite)
        elif varType == 'ptr':
            if endToken.token == '=':
                exprTokens = self.findExpr(self.EXPR_ASSIGN)
                return PtrDeclAST(nameToken, self.expr(exprTokens))
            elif endToken.token == ';':
                return PtrDeclAST(nameToken, Int12AST(endToken, 0))
        else:
            self.raiseInvalidToken(nameToken)

    def funcDeclaration(self, funcType, nameToken):
        args = self.findFuncParams()

        if self.getToken().token == '{':
            bodyTokens = self.findBody()
            #print('\nBODY')

            bodyAST = ASTParser(bodyTokens).run()

            #print('BODY END\n')

            return FuncDeclAST(funcType, nameToken, args, bodyAST)
        else:
            self.raiseInvalidToken(self.getToken())

    #
    # Variable operation
    #

    # TODO: ! operand
    def rawIdentifier(self):
        assignTypes = ['=', '+=', '-=', '&=', '|=', '^=']
        opTypes = [ '==', '!=', '<=', '>=', '+', '-', '<', '>', '&', '|', '^' ]

        assignVals = {
            '=' : 'assign',
            '+=': 'addAssign',
            '-=': 'subAssign',
            '&=': 'andAssign',
            '|=': 'orAssign',
            '^=': 'xorAssign'
        }

        nameToken = self.getToken()
        self.nextToken() # identifier -> =

        if self.getToken().token in assignTypes:
            assignType = self.getToken().token
            self.nextToken()

            expr = self.findExpr(self.EXPR_ASSIGN)

            return VarAssignAST(nameToken, assignVals[assignType], self.expr(expr))
        # Function call
        elif self.getToken().token == '(':
            args = self.findFuncArgs()
            return FuncCallAST(nameToken, args)
        elif self.getToken().token in opTypes:
            # Discard expression
            self.findExpr(self.EXPR_ASSIGN)
            return None
        else:
            self.raiseInvalidToken(self.getToken())

    #
    # Keywords
    #

    def intKeyword(self):
        self.nextToken() # int -> identifier

        if self.isIdentifier() and not self.isReserved():
            nameToken = self.getToken()
            self.nextToken() # identifier -> = ( ;

            # Variable declaration
            if self.getToken().token in '=;':
                return self.varDeclaration('int', nameToken)
            # Function declaration
            elif self.getToken().token == '(':
                return self.funcDeclaration('int', nameToken)
            else:
                self.raiseInvalidToken(self.getToken())
        else:
            raise ValueError('Invalid identifier name (expand).')

    def spriteKeyword(self):
        self.nextToken() # sprite -> identifier

        if self.isIdentifier() and not self.isReserved():
            nameToken = self.getToken()

            if self.nextToken().token == '=':
                return self.varDeclaration('sprite', nameToken)
            else:
                self.raiseInvalidToken(self.getToken())
        else:
            raise ValueError('Invalid identifier name (expand).')

    def voidKeyword(self):
        self.nextToken() # void -> identifier

        if self.isIdentifier() and not self.isReserved():
            nameToken = self.getToken()
            self.nextToken() # identifier -> (

            if self.getToken().token == '(':
                return self.funcDeclaration('void', nameToken)
            else:
                self.raiseInvalidToken(self.getToken())
        else:
            raise ValueError('Invalid identifier name (expand).')

    def ptrKeyword(self):
        self.nextToken() # ptr -> identifier

        if self.isIdentifier() and not self.isReserved():
            nameToken = self.getToken()
            self.nextToken() # identifier -> =

            if self.getToken().token == '=':
                return self.varDeclaration('ptr', nameToken)
            else:
                self.raiseInvalidToken(self.getToken())
        else:
            self.raiseInvalidToken(self.getToken())

    def whileKeyword(self):
        self.nextToken() # while -> (

        if self.getToken().token == '(':
            self.nextToken() # ( -> expr
            expr = self.findExpr(self.EXPR_LASTARG)

            if self.getToken().token == '{':
                bodyTokens = self.findBody()

                #print('\nWHILE')
                bodyAST = ASTParser(bodyTokens).run()
                #print('END WHILE\n')

                return WhileAST(self.expr(expr), bodyAST)
        else:
            self.raiseInvalidToken(self.getToken())

    def ifBlock(self, ifType):
        elseToken = self.getToken()
        self.nextToken() # [if else if else] -> (

        if ifType == 'else' and self.getToken().token == '{':
            bodyTokens = self.findBody()
            bodyAST = ASTParser(bodyTokens).run()

            return IfAST(Int8AST(elseToken, 1), bodyAST)
        elif ifType in ['if', 'else if'] and self.getToken().token == '(':
            self.nextToken() # ( -> expr
            expr = self.findExpr(self.EXPR_LASTARG)

            if self.getToken().token == '{':
                bodyTokens = self.findBody()
                bodyAST = ASTParser(bodyTokens).run()

                return IfAST(self.expr(expr), bodyAST)
            else:
                self.raiseInvalidToken(self.getToken())
        else:
            self.raiseInvalidToken(self.getToken())

    def ifKeyword(self):
        _if = self.ifBlock('if')
        root = _if

        while self.getToken() and self.getToken().token == 'else if':
            elseif = self.ifBlock('else if')
            _if.right = elseif
            _if = elseif

        if self.getToken() and self.getToken().token == 'else':
            _else = self.ifBlock('else')
            _if.right = _else

        return root

    def returnKeyword(self):
        self.nextToken() # return -> expr
        expr = self.findExpr(self.EXPR_ASSIGN)
        return ReturnAST(self.expr(expr))

    def skipComment(self, commentType):
        line = self.getToken().line

        if commentType == '//':
            while self.getToken() is not None and self.getToken().line == line:
                self.nextToken()
        elif commentType == '/*':
            while True:
                if self.getToken() is None:
                    raise ValueError('Unfinished multiline comment.')
                elif self.getToken().token == '*/':
                    self.nextToken()
                    break
                else:
                    self.nextToken()

    #
    # run
    #

    def run(self):
        execAST = ExecAST(None, None)
        root = execAST

        while self.getToken() is not None:
            if self.getToken().token == 'int':
                execAST.left = self.intKeyword()
            elif self.getToken().token == 'sprite':
                execAST.left = self.spriteKeyword()
            elif self.getToken().token == 'void':
                execAST.left = self.voidKeyword()
            elif self.getToken().token == 'ptr':
                execAST.left = self.ptrKeyword()
            elif self.getToken().token == 'while':
                execAST.left = self.whileKeyword()
            elif self.getToken().token == 'return':
                execAST.left = self.returnKeyword()
            elif self.getToken().token == 'if':
                execAST.left = self.ifKeyword()
            # Variable assignment, function call
            elif self.isIdentifier():
                ast = self.rawIdentifier()

                # None if its an expression on the wild
                if ast != None:
                    execAST.left = ast
            elif self.getToken().token == '//':
                self.skipComment('//')
            elif self.getToken().token == '/*':
                self.skipComment('/*')
            else:
                self.raiseInvalidToken(self.getToken())

            execAST.right = ExecAST(None, None)
            execAST = execAST.right

        return root
