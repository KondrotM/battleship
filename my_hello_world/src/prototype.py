import pdb
import random

if __name__ == '__main__':
    grid = []

    length = 10 

    for i in range(length):
        grid.append([0]*length)

    def place_battleship(grid, row, col, ship, orientation):
        if orientation == 'horizontal':

            if col + ship.length > len(grid[0]):
                raise ValueError('Ship is out of bounds')

            # if another ship is already there
            for i in range(ship.length):
                if grid[row][col + i] != 0:
                    raise ValueError('Another ship is already there')

            for i in range(ship.length):
                grid[row][col + i] = ship.name
        else:

            if row + ship.length > len(grid):
                raise ValueError('Ship is out of bounds')

            # if another ship is already there
            for i in range(ship.length):
                if grid[row + i][col] != 0:
                    raise ValueError('Another ship is already there')


            for i in range(ship.length):
                grid[row + i][col] = ship.name
        

    class Ship:
        def __init__(self, length, name):
            self.length = length
            self.name = name
            self.hits = 0
        

    def print_grid(grid):
        for row in grid:
            print(row)

    ships = [Ship(5, 'carrier'), Ship(4, 'battleship'), Ship(3, 'submarine'), Ship(3, 'cruiser'), Ship(2, 'destroyer')]


    for ship in ships:
        placed = False
        while not placed:
            try:
                row = random.randint(0, length - 1)
                col = random.randint(0, length - 1)
                orientation = random.choice(['horizontal', 'vertical'])
                place_battleship(grid, row, col, ship, orientation)
                placed = True
            except ValueError:
                pass


    # place_battleship(grid, 0, 0, battleship, 'horizontal')
    # place_battleship(grid, 1, 0, submarine, 'vertical')

    def attack(grid, row, col):
        if grid[row][col] == 0:
            print('Miss')
        else:
            print('Hit')
            grid[row][col] = 0

    pdb.set_trace()