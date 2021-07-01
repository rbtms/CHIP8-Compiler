VAR_TYPES = ['int', 'void', 'sprite', 'ptr']

CONSTANTS = [ 'SPRITE_0', 'SPRITE_1', 'SPRITE_2', 'SPRITE_3', 'SPRITE_4',
              'SPRITE_5', 'SPRITE_6', 'SPRITE_7', 'SPRITE_8', 'SPRITE_9',
              'SPRITE_A', 'SPRITE_B', 'SPRITE_C', 'SPRITE_D', 'SPRITE_E',
              'SPRITE_F' ]

BUILTINS = [ 'clearScreen', 'draw', 'printN', 'print1N', 'print2N', 'Sprite', '_BCD', 'isKeyPressed',
             '_getDelayTimer', '_setSoundTimer', '_setDelayTimer',
             'waitForKeypress', 'beep', 'randInt', 'spriteN', 'sleep' ]

OPERANDS = [ '==', '!=', '\+=', '\-=', '<=', '>=', '\&=', '\|=', '\^=', '\*=',
              '=', '!', '\+', '\-', '<', '>', '\&\&', '\&', '\|\|', '\|', '\^',
             '\*', '@', '#' ]

COMMA = [ ',' ]
SEMICOLON  = [ ';' ]
BRACKETS  = [ '\(', '\)', '\{', '\}' ]
CONSTRUCTS = [ 'else if', 'if', 'else', 'while', 'return' ]

COMMENTS = [ '//', '/\*', '\*/' ]

RESERVED = VAR_TYPES + CONSTANTS + BUILTINS + CONSTRUCTS
