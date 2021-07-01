import keyboard
import os
os.environ['PYGAME_HIDE_SUPPORT_PROMPT'] = "hide"

import pygame
from pygame.locals import QUIT

class Screen:
    def __init__(self, headless):
        self.W = 64
        self.H = 32
        self.PIXEL_SIZE = 10
        self.WHITE = (255, 255, 255)
        self.BLACK = (0, 0, 0)

        self.CHAR_CODES = { pygame.K_0: 0x00, pygame.K_KP_0: 0x00,
                            pygame.K_1: 0x01, pygame.K_KP_1: 0x01,
                            pygame.K_2: 0x02, pygame.K_KP_2: 0x02,
                            pygame.K_3: 0x03, pygame.K_KP_3: 0x03,
                            pygame.K_4: 0x04, pygame.K_KP_4: 0x04,
                            pygame.K_5: 0x05, pygame.K_KP_5: 0x05,
                            pygame.K_6: 0X06, pygame.K_KP_6: 0x06,
                            pygame.K_7: 0x07, pygame.K_KP_7: 0x07,
                            pygame.K_8: 0x08, pygame.K_KP_8: 0x08,
                            pygame.K_9: 0x09, pygame.K_KP_9: 0x09,
                            pygame.K_a: 0x0A, pygame.K_b: 0x0B,
                            pygame.K_c: 0x0C, pygame.K_d: 0x0D,
                            pygame.K_e: 0x0E, pygame.K_f: 0x0F,
                            pygame.K_q: -1, pygame.K_UP: -2, pygame.K_DOWN: -3, pygame.K_LEFT: -4 }

        self.screen = None
        self.buf = [ [ False for x in range(self.W) ] for y in range(self.H)  ]
        self.headless = headless

        self.init()

    def init(self):
        pygame.init()
        pygame.display.set_icon(pygame.Surface((0, 0)))
        pygame.display.set_caption('CHIP8')

        if not self.headless:
            self.screen = pygame.display.set_mode((self.PIXEL_SIZE*self.W, self.PIXEL_SIZE*self.H))

    def clear(self):
        for y in range(self.H):
            for x in range(self.W):
                self.buf[y][x] = False

        self.screen.fill(self.BLACK)

    def refresh(self):
        pygame.display.update()

    def isPixel(self, x, y):
        return self.buf[y%self.H][x%self.W]

    def drawPixel(self, x, y, b):
        if b:
            _y = y%self.H
            _x = x%self.W
            self.buf[_y][_x] ^= 1

            pygame.draw.rect(self.screen,
                             self.WHITE if self.buf[_y][_x] else self.BLACK,
                             (_x*self.PIXEL_SIZE, _y*self.PIXEL_SIZE, self.PIXEL_SIZE, self.PIXEL_SIZE)
            )

            return self.buf[_y][_x] == 0
        else:
            return False

    def getChar(self):
        for event in pygame.event.get():
            if event.type == QUIT:
                return -1
            elif event.type == pygame.KEYDOWN:
                if event.key in self.CHAR_CODES:
                    return self.CHAR_CODES[event.key]

        return None

    def isKeyPressed(self, code):
        char = None

        if code < 0: # -1
            char = 'Q'
        elif code < 0x0A:
            char = chr(code + ord('0'))
        else:
            char = chr(code-0x0A + ord('A'))

        #print(' '*100 + 'IS', hex(code), 'pressed: ', keyboard.is_pressed(char) or keyboard.is_pressed(char.lower()))

        return keyboard.is_pressed(char) or keyboard.is_pressed(char.lower())

