import sys
print(1)
import tokenizer
import ASTparser as AST
import asmParser as ASM

def main():
    path = 'draw._t' if len(sys.argv) == 1 else sys.argv[1]
    program = open(path, 'r').read()

    print()
    print('TOKENIZER')
    print('------------------------------------------------------')
    t = tokenizer.Tokenizer(program)
    tokens = t.run()

    print()
    print('AST')
    print('------------------------------------------------------')

    a = AST.ASTParser(tokens)
    ast = a.run()
    AST.printAST(ast, 0)

    print()
    print('ASM PARSER')
    print('------------------------------------------------------')

    asmParser = ASM.ASMParser(ast)
    asmParser._run()
    asmParser.emitCode('target.ch8')

main()
