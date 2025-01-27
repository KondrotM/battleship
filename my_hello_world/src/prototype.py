import pdb
import random
import time

class Ship:
    def __init__(self, length, name):
        self.length = length
        self.name = name
        self.tiles = []
        self.hits = 0

 
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
            ship.tiles.append((row, col + i, False))
    else:

        if row + ship.length > len(grid):
            raise ValueError('Ship is out of bounds')

        # if another ship is already there
        for i in range(ship.length):
            if grid[row + i][col] != 0:
                raise ValueError('Another ship is already there')


        for i in range(ship.length):
            grid[row + i][col] = ship.name
            ship.tiles.append((row + i, col, False))

def place_all_ships(grid, ships):
    for ship in ships:
        placed = False
        while not placed:
            try:
                row = random.randint(0, len(grid) - 1)
                col = random.randint(0, len(grid[0]) - 1)
                orientation = random.choice(['horizontal', 'vertical'])
                place_battleship(grid, row, col, ship, orientation)
                placed = True
            except ValueError:
                pass

def attack(grid, visible_grid, ships, row, col):
    tile = grid[row][col]

    if tile == 0:
        print('Miss')
        visible_grid[row][col] = 'M'
        return False
    
    for ship in ships:
        if ship.name == tile:
            print(f'Hit {ship.name}')
            ship.hits += 1
            visible_grid[row][col] = 'H'
            for i in range(len(ship.tiles)):
                if ship.tiles[i][0] == row and ship.tiles[i][1] == col:
                    ship.tiles[i] = (row, col, True)
                    break
            if ship.hits == ship.length:
                print(f'{ship.name} has been sunk')
            return (ship.name)



def print_grid(grid):
    for row in grid:
        print(' '.join(str(cell) for cell in row))

def ai_take_turn(visible_grid, ai_memory):

    rand = True

    for ship in ai_memory.keys():
        if len(ai_memory[ship]['hits']) == ai_memory[ship]['length']:
            print('Ship already sunk')
            continue
            
        if len(ai_memory[ship]['hits']) == 0:
            print('No hits yet')
            continue

        rand = False

        if len(ai_memory[ship]['hits']) == 1:
            # try to attack around the hit
            hit = ai_memory[ship]['hits'][0]

            directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
            chosen_direction = random.choice(directions)

            row = hit[0] + chosen_direction[0]
            col = hit[1] + chosen_direction[1]

            while row < 0 or row >= len(visible_grid) or col < 0 or col >= len(visible_grid[0]) or visible_grid[row][col] != '~':
                chosen_direction = random.choice(directions)
                row = hit[0] + chosen_direction[0]
                col = hit[1] + chosen_direction[1]

            break

        else: # there are at least two hits

            hits = ai_memory[ship]['hits'].copy()

            if hits[0][0] == hits[1][0]: # if x axis is the same
                direction = 'horizontal'

                # sort hits by x axis
                hits.sort(key=lambda x: x[1])

                chosen_tile = None

                # first try to increase
                row = hits[-1][0]
                col = hits[-1][1] + 1

                # then decrease
                if row < 0 or row >= len(visible_grid) or col < 0 or col >= len(visible_grid[0]) or visible_grid[row][col] != '~':
                    row = hits[0][0]
                    col = hits[0][1] - 1

                chosen_tile = (row, col)


            if hits[0][1] == hits[1][1]: # if y axis is the same
                direction = 'vertical'

                hits.sort(key=lambda x: x[0])


                chosen_tile = None

                # first try to increase
                row = hits[-1][0] + 1
                col = hits[-1][1] 

                # then decrease
                if row < 0 or row >= len(visible_grid) or col < 0 or col >= len(visible_grid[0]) or visible_grid[row][col] != '~':
                    row = hits[0][0] - 1 
                    col = hits[0][1]

                chosen_tile = (row, col)


                # pdb.set_trace()
            

            

            



            # try to attack in the same direction

            # use math to determine direction

            # hit_direction = (ai_memory[ship]['hits'][0][0] - ai_memory[ship]['hits'][1][0], ai_memory[ship]['hits'][0][1] - ai_memory[ship]['hits'][1][1])
            # pdb.set_trace()

            # try to follow direction




            # hit1 = ai_memory[ship]['hits'][0]
            # hit2 = ai_memory[ship]['hits'][1]

            # if hit1[0] == hit2[0]:
            #     # horizontal
            #     if hit1[1] < hit2[1]:
            #         # right
            #         row = hit2[0]
            #         col = hit2[1] + 1
            #     else:
            #         # left
            #         row = hit2[0]
            #         col = hit2[1] - 1
            # else:
            #     # vertical
            #     if hit1[0] < hit2[0]:
            #         # down
            #         row = hit2[0] + 1
            #         col = hit2[1]
            #     else:
            #         # up
            #         row = hit2[0] - 1
            #         col = hit2[1]

            # while row < 0 or row >= len(visible_grid) or col < 0 or col >= len(visible_grid[0]) or visible_grid[row][col] != '~':
            #     row, col = random_move(visible_grid, rand=True)

            #     break



    if rand:
        row = random.randint(0, len(visible_grid) - 1)
        col = random.randint(0, len(visible_grid[0]) - 1)

        while visible_grid[row][col] != '~':
            row = random.randint(0, len(visible_grid) - 1)
            col = random.randint(0, len(visible_grid[0]) - 1)

    return row, col

if __name__ == '__main__':
    player1_grid = []
    player1_visible_grid = []
    player1_ships = [Ship(5, 'carrier'), Ship(4, 'battleship'), Ship(3, 'submarine'), Ship(3, 'cruiser'), Ship(2, 'destroyer')]
    ai1_memory = {
        'carrier': {
            'length': 5,
            'hits': [],
        },
        'battleship': {
            'length': 4,
            'hits': [],
        },
        'submarine': {
            'length': 3,
            'hits': [],
        },
        'cruiser': {
            'length': 3,
            'hits': [],
        },
        'destroyer': {
            'length': 2,
            'hits': [],
        }
    }
    

    player2_grid = []
    player2_visible_grid = []
    player2_ships = [Ship(5, 'carrier'), Ship(4, 'battleship'), Ship(3, 'submarine'), Ship(3, 'cruiser'), Ship(2, 'destroyer')]
    ai2_memory = {
        'carrier': {
            'length': 5,
            'hits': [],
        },
        'battleship': {
            'length': 4,
            'hits': [],
        },
        'submarine': {
            'length': 3,
            'hits': [],
        },
        'cruiser': {
            'length': 3,
            'hits': [],
        },
        'destroyer': {
            'length': 2,
            'hits': [],
        }
    }
 

    grid_size = 10 

    for i in range(grid_size):
        player1_grid.append([0]*grid_size)
        player1_visible_grid.append(['~']*grid_size)

        player2_grid.append([0]*grid_size)
        player2_visible_grid.append(['~']*grid_size)


    place_all_ships(player1_grid, player1_ships)
    place_all_ships(player2_grid, player2_ships)


    player = 0
    while not all(ship.hits == ship.length for ship in player1_ships) or not all(ship.hits == ship.length for ship in player2_ships):
        if player == 0:

            print('PLAYER 1 TURN')
            time.sleep(.1)

            coords = ai_take_turn(player2_visible_grid, ai1_memory)

            out = attack(player2_grid, player2_visible_grid, player2_ships, coords[0], coords[1])

            if out:
                ai1_memory[out]['hits'].append((coords[0], coords[1]))


            print('AI 1 ATTACKED %s %s' % (coords[0], coords[1]))
            time.sleep(.1)

            print_grid(player2_visible_grid)

            if not out:
                player = 1

        else:
            print ('PLAYER 2 TURN')

            coords = ai_take_turn(player1_visible_grid, ai2_memory)

            out = attack(player1_grid, player1_visible_grid, player1_ships, coords[0], coords[1])

            if out:
                ai2_memory[out]['hits'].append((coords[0], coords[1]))


            print('AI 2 ATTACKED %s %s' % (coords[0], coords[1]))
            time.sleep(.1)


            # inp = input(f'Player {player + 1} enter row and col: ')
            # if inp == 'q':
            #     break
            # row, col = inp.split(' ')

            # out = attack(player1_grid, player1_visible_grid, player1_ships, int(row), int(col))
            print_grid(player1_visible_grid)

            if not out:
                player = 0


    print('Goodbye')


    # player = 0
    # while True:
    #     print_grid(visible_grid)
    #     row = int(input('Enter row: '))
    #     col = int(input('Enter col: '))

    #     attack(grid, visible_grid, row, col)

    #     if all(all(tile[2] for tile in ship.tiles) for ship in ships):
    #         print('Game over')
    #         break


    pdb.set_trace()