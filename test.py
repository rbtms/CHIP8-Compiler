import sys
sys.path.append('compiler')
sys.path.append('emulator')

import unittest
import ASTparser as AST
import tokenizer as tokenizer
import asmParser as ASM
from emulator.chip8 import Interpreter, loadProgram

# TODO: Check why it doesnt give an error when two numbers are together
class TokenizerTest(unittest.TestCase):
    def _test(self, code, expected):
        tokens = tokenizer.Tokenizer(code, False).run()
        self.assertEqual( list(map(lambda t: t.token, tokens)), expected )

    def test_twoNumbers(self):
        code = '2+3'
        expected = ['2', '+', '3']

        self._test(code, expected)

    def test_math(self):
        code = '!2+3-4>5<(6&7)|(8^(9))*10'
        expected = ['!', '2', '+', '3', '-', '4', '>', '5', '<', '(', '6', '&', '7', ')', '|',
             '(', '8',  '^', '(', '9', ')', ')', '*', '10']

        self._test(code, expected)

    def test_varTypesConstants(self):
        code = 'sprite test = SPRITE_0;'\
             + 'int a = 0;'\
             + 'int b = 0xFF;'\
             + 'ptr p = BCD(1);'\
             + 'sprite a = b00000001 b10101010;'\
             + 'int foo = 1234;'

        expected = ['sprite', 'test', '=', 'SPRITE_0', ';',
                  'int', 'a', '=', '0', ';',
                  'int', 'b', '=', '0xFF', ';',
                  'ptr', 'p', '=', 'BCD', '(', '1', ')', ';',
                  'sprite', 'a', '=', 'b00000001', 'b10101010', ';',
                  'int', 'foo', '=', '1234', ';']

        self._test(code, expected)

class ASTTest(unittest.TestCase):
    def _test(self, code, expected):
        tokens = tokenizer.Tokenizer(code, False).run()
        ast    = AST.ASTParser(tokens).run()

        self.assertEqual(str(ast.left), expected)

    def test_declTest(self):
        code = 'int a = 2+3;'
        expected = 'IntDecl a [OpAST(sum Int8(2) Int8(3))]'

        self._test(code, expected)

    def test_assigTest(self):
        code = 'test = 0xFF;'
        expected = 'VarAssign assign test [Int8(255)]'

        self._test(code, expected)

    def test_assignFunc(self):
        code = 'test += foo();'
        expected = 'VarAssign addAssign test [FuncCall foo 0 <[]>]'

        self._test(code, expected)

    # TODO: Test nested functions
    def test_assignFuncParams(self):
        code = 'test &= foo(1, 2);'
        expected = 'VarAssign andAssign test [FuncCall foo 2 <[\'FuncArg Int8(1)\', \'FuncArg Int8(2)\']>]'

        self._test(code, expected)

    def test_return(self):
        code = 'return 2;'
        expected = 'Return Int8(2)'

        self._test(code, expected)

class ExecTest(unittest.TestCase):
    def _testVars(self, code, vars, verbose=False):
        tokens = tokenizer.Tokenizer(code, False).run()

        astParser = AST.ASTParser(tokens)
        ast = astParser.run()

        if verbose:
            AST.printAST(ast, 0)

        asm    = ASM.ASMParser(ast, verbose=verbose)
        asm._run()

        #asm.emitCode('test.ch8')
        #program = loadProgram('test.ch8')

        interpreter = Interpreter(asm.mem, verbose=verbose, headless=True)
        interpreter.run()


        for var in vars:
            self.assertEqual(interpreter.mem[ asm.vars[var].addr+asm.MEM_OFF ], vars[var])

    def test_decl(self):
        code = 'int a = 1;'
        self._testVars(code, { 'a': 1 })

    #
    # Expression parsing
    #

    def test_sum(self):
        code = 'int a; a = 2+3;'
        self._testVars(code, { 'a': 5 })

    def test_sub(self):
        code = 'int a = 10; int b = 4; int res = a-b;'
        self._testVars(code, { 'res': 6 })

    def test_and(self):
        code = 'int a = 6&1; int b = 7&3&1;'
        self._testVars(code, { 'a': 0, 'b': 1 })

    def test_or(self):
        code = 'int a = 2|1; int b = 0|0;'
        self._testVars(code, { 'a': 3, 'b': 0 })

    def test_xor(self):
        code = 'int a = 2^5;'
        self._testVars(code, { 'a': 7 })

    def test_lt(self):
        code = 'int t = 2 < 3; int f = 3 < 2; int ff = 2 < 2;'
        self._testVars(code,  { 't': 1, 'f': 0, 'ff': 0 })

    def test_gt(self):
        code = 'int t = 3 > 2; int f = 2 > 3; int ff = 2 > 2;'
        self._testVars(code,  { 't': 1, 'f': 0, 'ff': 0 })

    def test_let(self):
        code = 'int t = 2 <= 3; int f = 3 <= 2; int tt = 2 <= 2;'
        self._testVars(code,  { 't': 1, 'f': 0, 'tt': 1 })

    def test_get(self):
        code = 'int t = 3 >= 2; int f = 2 >= 3; int tt = 2 >= 2;'
        self._testVars(code,  { 't': 1, 'f': 0, 'tt': 1 })

    def test_eq(self):
        code = 'int t = 2 == 2; int f = 2 == 3; int tt = 0 == 0;'
        self._testVars(code,  { 't': 1, 'f': 0, 'tt': 1 })

    def test_neq(self):
        code = 'int f = 2 != 2; int t = 2 != 3; int ff = 0 != 0;'
        self._testVars(code,  { 'f': 0, 't': 1, 'ff': 0 })

    def test_logAnd(self):
        code = 'int t = 1 && 1; int f = 1 && 0; int ff = 0 && 0;'
        self._testVars(code,  { 't': 1, 'f': 0, 'ff': 0 })

    def test_logOr(self):
        code = 'int t = 1 || 1; int tt = 1 || 0; int ttt = 0 || 1; int f = 0 || 0;'
        self._testVars(code,  { 't': 1, 'tt': 1, 'ttt': 1, 'f': 0})

    def test_parens(self):
        code = 'int a = ((1+2)+3) + ((4+5) + 6);'
        self._testVars(code, { 'a': 21 })


if __name__ == '__main__':
    unittest.main()
