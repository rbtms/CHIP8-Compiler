import re
import tokenNames as TN

class Token:
    def __init__(self, token, line, pos):
        self.token = token
        self.line  = line
        self.pos   = pos

    def __str__(self):
        return 'Token(' + self.token + ')'

class Tokenizer:
    def __init__(self, code, verbose=True):
        self.code = code
        self.verbose = verbose

    def genTokens(self):
        regex = re.compile(
            '|'.join(TN.BUILTINS)         + '|' + '|'.join(TN.CONSTANTS)  + '|' + '|'.join(TN.VAR_TYPES)
            + '|' + '|'.join(TN.COMMENTS) + '|' + '|'.join(TN.COMMA)      + '|' + '|'.join(TN.SEMICOLON)
            + '|' + '|'.join(TN.BRACKETS) + '|' + '|'.join(TN.CONSTRUCTS) + '|' + '|'.join(TN.OPERANDS)
            + '|' + 'b\d+'              # Binary
            + '|' + '0x[\d\w]+'         # Hex
            + '|' + '[_a-zA-Z][\w\d]*'  # Words
            + '|' + '\d+'               # Dec
        )

        tokens = regex.findall(self.code)

        return tokens

    def findTokens(self, tokens):
        _tokens = []

        # TODO: Check \r\n
        lines = self.code.split('\n')

        for lineN, line in enumerate(lines):
            pos = 0

            while pos < len(line):
                if line[pos] == ' ' or line[pos] == '\n':
                    pos += 1
                elif line[pos:pos+len(tokens[0])] == tokens[0]:
                    if self.verbose:
                        print('Token found:', tokens[0], 'at', (lineN+1, pos+1))

                    _tokens.append( Token(tokens[0], lineN+1, pos+1) )

                    pos += len(tokens[0])
                    tokens.pop(0)
                else:
                    raise IndexError('Incorrect token in position', (lineN+1, pos+1))

        return _tokens

    def run(self):
        tokens = self.genTokens()
        tokens = self.findTokens(tokens)
        return tokens
