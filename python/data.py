
from collections import defaultdict

COLORS = ['RED', 'BLUE', 'GREEN', 'BLACK', 'WHITE']
TOKENS = COLORS + ['GOLD']

def abbreviate_color(color_string):
    if color_string not in COLORS:
        raise RuntimeException('Unknown color {}'.format(color_string))
    elif color_string != 'BLACK':
        return color_string[0]
    else:
        return 'K'

def expand_color(color_char):
    if color_char == 'R':
        return 'RED'
    elif color_char == 'B':
        return 'BLUE'
    elif color_char == 'G':
        return 'GREEN'
    elif color_char == 'K':
        return 'BLACK'
    elif color_char == 'W':
        return 'WHITE'
    else:
        raise RuntimeException('Unknown color char {}'.format(color_char))

class Card(object):
    def __init__(self, color, score, cost):
        self.color = color
        self.score = score
        self.cost = cost

    def __repr__(self):
        return '%d%s @ %s' % (self.score, abbreviate_color(self.color), self.cost)

    def discounted_cost(self, discount):
        discounted = {}
        for color, cost in self.cost.iteritems():
            if cost > discount.get(color, 0):
                discounted = cost - discount.get(color, 0)
        return discounted

class Noble(object):
    def __init__(self, score, requirement):
        self.score = score
        self.requirement = requirement

    def __repr__(self):
        return 'Noble(%d @ %s)' % (self.score, self.requirement)

class Deck(object):
    def __init__(self, cards, revealed=4):
        self.cards = cards
        self.revealed = revealed

    def cards_visible(self):
        return self.cards[:self.revealed]

    def card_at(self, index):
        return self.cards[index]

    def remove_card_at(self, index):
        return self.pop(index)

class Board(object):
    def __init__(self, tokens, decks, nobles):
        self.tokens = tokens
        self.decks = decks
        self.nobles = nobles

    @staticmethod
    def default_board():
        # TODO
        return Board([], [], [])
