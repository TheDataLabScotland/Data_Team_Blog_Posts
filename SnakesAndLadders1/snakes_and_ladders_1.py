import numpy as np
import sys


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

p1 = p2 = 0

while True:
    p1 = roll_die(p1)
    if p1 >= 100:
        print "Player 1 wins!"
        sys.exit(0)
    p2 = roll_die(p2)
    if p2 >= 100:
        print "Player 2 wins!"
        sys.exit(0)