import math
import random
import sys
from time import sleep
from turtle import pos
from typing import Dict, Generator, List, Tuple

from attr import dataclass

TIME_DELAY_FOR_INFO = 1
sys.setrecursionlimit(5000)  # for large fields more recursion is needed than 500

@dataclass(slots=True, init=True, hash=True, eq=True)
class Point:
    x: int
    y: int

def print_field(f: List[List[int]]) -> None:
    nll = max(max(f, key=lambda l: max(l))) // 10 + 1 # + 1 done by , in print
    for line in f:
        for i, c in enumerate(line):
            print(" " * (nll - len(str(c))), c, end="", sep=" " if i != 0 else "")
        print()

def print_field_pairs(n: int, pairs: Dict[int, Tuple[Point, Point]]) -> None:
    dummy_field = [[0 for i in range(n)] for j in range(n)]
    for pairN in pairs:
        pos1, pos2 = pairs[pairN]
        dummy_field[pos1.y][pos1.x] = pairN
        dummy_field[pos2.y][pos2.x] = pairN
    
    print_field(dummy_field)


def get_neighbours(pos: Point, field) -> Generator[Point, None, None]:
    for dy in range(-1, 2, 1):
        for dx in range(-1, 2, 1):
            if pos.x + dx < 0 or pos.x + dx >= len(field) or pos.y + dy < 0 or pos.y + dy >= len(field) or abs(dx) + abs(dy) == 2:
                continue

            n = Point(pos.x + dx, pos.y + dy)
            if field[n.y][n.x] == 0:
                yield n

    
def get_reachable_positions(start: Point, field: List[List[int]], data: Dict[Point, List[Point]]) -> Dict[Point, List[Point]]:
    if not data:
        data[start] = [start]
    for neighbour in get_neighbours(start, field):
        if data.get(neighbour):
            if len(data[start]) + 1 >= len(data[neighbour]):
                continue
        data[neighbour] = data[start][:] + [neighbour]
        data.update(get_reachable_positions(neighbour, field, data))
    return data
    

def solve(n: int, pairs: int, depth=50):
    if depth <= 0:
        print("INFO: Failed finding a solution within 50 tries")
        return
        
    if n < 4 or int(n // 2 + 0.5) > pairs:
        print("N < 4 or too less pairs")
        return
        
    field = [[0 for _ in range(n)] for _ in range(n)]

    positions: Dict[int, Tuple[Point, Point]] = {}
    free_positions = [Point(x, y) for y in range(0, n) for x in range(0, n)]
    size_pair_ratio = math.ceil(n * n / (2 * pairs))
    if size_pair_ratio <= 2:
        print("INFO: Large amount of pairs required.")
        if size_pair_ratio == 1:
            print("INFO: The result would require every pair next to each other. This algorithm can't solve such fields (probably). This field wouldn't make sense, as well")
        else:
            print("INFO: The resulting field would not have a high complexity, therefore this algorithm may fail")
        sleep(TIME_DELAY_FOR_INFO)
        
    print(f"{size_pair_ratio=}")

    while len(positions) < pairs:
        if not free_positions:
            print("Failure: No free positions anymore.\nCurrent result:")
            print_result(n, len(positions), positions, field)  # len(positions) for pairs to have the correct amount
            print("INFO: Next try")
            solve(n, pairs, depth-1)
            return
            
        pos1 = random.choice(free_positions)
        reachable_positions: Dict[Point, List[Point]] = get_reachable_positions(pos1, field, {})
        
        reachable_positions.pop(pos1)   # only help for the funtion
        if not reachable_positions:
            # single closed field with no neighbours
            free_positions.remove(pos1)
            if not free_positions:  # may happen in small fields and large amount of pairs
                print("FAILURE: Creating arukon. Retrying...")
                solve(n, pairs, depth-1)
                return
            continue

        # larger distances = more difficult to solve
        lpositions = sorted(list(reachable_positions), key=lambda x: len(reachable_positions[x]), reverse=True)
        if len(lpositions) > 5:
            lpositions = lpositions[:size_pair_ratio]
        
        # remove distances of size_pair_ratio or lower if possible  (n=6; pairs=5) = ceil(36 / 10) = 4  (distances of 4 or lower) 
        for i in range(len(lpositions) - 1, -1, -1):
            if len(lpositions) == 1:
                break
            elif len(reachable_positions[lpositions[i]]) - 1 <= size_pair_ratio:
                lpositions.pop(i)

        index = random.randint(0, len(lpositions) - 1)
        pos2 = lpositions[index]
        
        for p in reachable_positions[pos2]:
            field[p.y][p.x] = pairs - len(positions)
            free_positions.remove(p)
        positions[pairs - len(positions)] = (pos1, pos2)

    print_result(n, pairs, positions, field)


def print_result(n, pairs, positions, field):
    print(n, pairs, end="\n", sep="\n")
    print_field_pairs(n, positions)
    print()
    print("Solved field:")
    print_field(field)
   
   
def main(*sizes: str):
    if not sizes:
        sn = ""
        pn = ""
        while not sn.isnumeric():
            sn = input("Size of n: ")
        n = int(sn)
        while not pn.isnumeric():
            pn = input("Amount of pairs: ")
        p = int(pn)
        solve(n, p)
    
    else:
        if len(sizes) % 2 == 1:
            print("WARNING: Ignoring last argument '" + sizes[-1] + "' because for this size has no defined pairs")
            sizes = sizes[:-1]
            
        for i, n in enumerate(sizes[::2]):
            solve(int(n), int(sizes[2 * i + 1]))
            if i != len(sizes) - 1:
                print("----------------------\n")
            

if __name__ == "__main__":
    main(*sys.argv[1:])
