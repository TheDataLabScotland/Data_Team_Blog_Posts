import numpy as np
from collections import Counter
import matplotlib.pyplot as plt


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
ply_count = Counter()
squares_count = Counter()

np.random.seed(42)

NUM_GAMES = 1000000

for games in range(NUM_GAMES):
    p1 = p2 = 0
    ply = 0
    while True:
        p1 = roll_die(p1)
        ply += 1
        if p1 > 100:
            squares_count[100] += 1
            p1_wins += 1
            ply_count[ply] += 1
            break
        else:
           squares_count[p1] += 1
        p2 = roll_die(p2)
        ply += 1
        if p2 >= 100:
            squares_count[100] += 1
            p2_wins += 1
            ply_count[ply] += 1
            break
        else:
            squares_count[p2] += 1
        
top_of_ladder = [16, 22, 33, 37, 51, 61, 74, 92, 95, 98]
tail_of_snake = [3, 7, 9, 11, 15, 23, 25, 44, 48, 58, 65]

ladder_tops = {k: squares_count[k] for k in top_of_ladder}
snake_tails = {k: squares_count[k] for k in tail_of_snake}
unmarked = {k: squares_count[k] for k in range(1, 101) if k not in top_of_ladder + tail_of_snake}

plt.bar(ladder_tops.keys(), ladder_tops.values(), 0.8, color="green", label="ladder top")
plt.bar(snake_tails.keys(), snake_tails.values(), 0.8, color="red", label="snake tail")
plt.bar(unmarked.keys(), unmarked.values(), 0.8, color="blue", label="unmarked")
plt.xlim([0, 101])
plt.ylim([0, 2500000])
plt.xlabel("Square")
plt.ylabel("Frequency")
plt.title("Distribution of Squares Landed On")
plt.legend(loc="upper right")
plt.draw()

plt.figure()
plt.bar(ply_count.keys(), ply_count.values(), 0.5, color="blue")
plt.xlim([0, 601])
plt.ylim([0, 18000])
plt.xlabel("Total Ply")
plt.ylabel("Frequency")
plt.title("Distribution of Half-Turns")
plt.draw()

plt.show()