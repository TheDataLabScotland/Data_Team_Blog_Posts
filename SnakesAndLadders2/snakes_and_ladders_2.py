import numpy as np


def snakes_and_ladders(x):
    dict_sal = {21:3, 24:7, 35:9, 50:11, 53:15, 60:23,
                75:44, 89:48, 93:25, 97:65, 99:58,
                4:16, 12:33, 18:22, 26:37, 42:61,
                49:51, 55:74, 82:98, 85:95, 88:92}
    return dict_sal.get(x, x)
    
def roll_die(x):
    x += np.random.randint(1, 7)
    x = snakes_and_ladders(x)
    return x

p1_wins = p2_wins = 0

np.random.seed(42)

NUM_GAMES = 1000000

for game in range(NUM_GAMES):
    p1 = p2 = 0
    while True:
        p1 = roll_die(p1)
        if p1 >= 100:
            p1_wins += 1
            break
        p2 = roll_die(p2)
        if p2 >= 100:
            break

print p1_wins, p2_wins