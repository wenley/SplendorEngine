
from board import Board
from player import Player

class Game(object):
    def __init__(self, players, board):
        self.players = players
        self.board = board

    @staticmethod
    def default_game():
        player_count = int(raw_input('Number of players: '))
        players = []
        for i in xrange(player_count):
            players.append(Player(raw_input('  Name of Player #%d: ' % (i+1,))))
        return Game(players, Board.default_board())
