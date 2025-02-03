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

    if visible_grid[row][col] != '~':
        print('Already attacked')
        return 'Invalid'

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
            # ship already sunk
            continue
            
        if len(ai_memory[ship]['hits']) == 0:
            # no hits yet
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

                # sort hits by x axis
                hits.sort(key=lambda x: x[1])

                # first try to increase
                row = hits[-1][0]
                col = hits[-1][1] + 1

                # then decrease
                if row < 0 or row >= len(visible_grid) or col < 0 or col >= len(visible_grid[0]) or visible_grid[row][col] != '~':
                    row = hits[0][0]
                    col = hits[0][1] - 1

                chosen_tile = (row, col)


            if hits[0][1] == hits[1][1]: # if y axis is the same

                hits.sort(key=lambda x: x[0])

                # first try to increase
                row = hits[-1][0] + 1
                col = hits[-1][1] 

                # then decrease
                if row < 0 or row >= len(visible_grid) or col < 0 or col >= len(visible_grid[0]) or visible_grid[row][col] != '~':
                    row = hits[0][0] - 1 
                    col = hits[0][1]

    if rand:
        row = random.randint(0, len(visible_grid) - 1)
        col = random.randint(0, len(visible_grid[0]) - 1)

        while visible_grid[row][col] != '~':
            row = random.randint(0, len(visible_grid) - 1)
            col = random.randint(0, len(visible_grid[0]) - 1)

    return row, col

def print_grids(opponent_grid, player_grid, opponent_visible_grid):

    nice_player_grid = []
    for i in range(len(player_grid)):
        grid_row = []
        for j in range(len(player_grid[0])):
            if player_grid[i][j] == 0:
                grid_row.append('~')
            else:
                grid_row.append('X')
            if opponent_visible_grid[i][j] != '~':
                grid_row[j] = 'O'
        nice_player_grid.append(grid_row)
    print('Opponent Grid                  Player Grid')
    for i in range(len(opponent_grid)):
        print(' '.join(str(cell) for cell in opponent_grid[i]) + '    ' + ' '.join(str(cell) for cell in nice_player_grid[i]))
 


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

            print('YOUR TURN')

            print_grids(player2_visible_grid, player1_grid, player1_visible_grid)

            out = 'Invalid'

            while out == 'Invalid':
                inp = input(f'Player {player + 1} enter row and col: ')
                if inp == 'q':
                    break
                
                # if inp cannot be split into two integers
                # continue
                # and other input validation below

                if ' ' not in inp:
                    print ('Invalid input')
                    continue

                if not inp.split(' ')[0].isdigit() or not inp.split(' ')[1].isdigit():
                    print ('Invalid input')
                    continue

                if int(inp.split(' ')[0]) < 0 or int(inp.split(' ')[0]) >= grid_size or int(inp.split(' ')[1]) < 0 or int(inp.split(' ')[1]) >= grid_size:
                    print ('Invalid input')
                    continue

                if player2_visible_grid[int(inp.split(' ')[0])][int(inp.split(' ')[1])] != '~':
                    print('Already attacked')
                    continue

                row, col = inp.split(' ')

                out = attack(player2_grid, player2_visible_grid, player2_ships, int(row), int(col))



            time.sleep(1)

            # coords = ai_take_turn(player2_visible_grid, ai1_memory)

            # out = attack(player2_grid, player2_visible_grid, player2_ships, coords[0], coords[1])

            # if out:
            #     ai1_memory[out]['hits'].append((coords[0], coords[1]))


            # print('AI 1 ATTACKED %s %s' % (coords[0], coords[1]))
            # time.sleep(.1)


            if not out:
                player = 1

        else:
            print ('ENEMY TURN')

            time.sleep(1)
            coords = ai_take_turn(player1_visible_grid, ai2_memory)

            print('ENEMY ATTACKED %s %s' % (coords[0], coords[1]))
            time.sleep(2)
            out = attack(player1_grid, player1_visible_grid, player1_ships, coords[0], coords[1])

            if out:
                ai2_memory[out]['hits'].append((coords[0], coords[1]))

            time.sleep(1)

            if not out:
                player = 0


    print('YOU WIN' if player == 0 else 'ENEMY WINS')
    print('Goodbye')



    pdb.set_trace()