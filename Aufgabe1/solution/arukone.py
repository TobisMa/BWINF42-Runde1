import random
import sys
from typing import Dict, Generator, List, Tuple

from attr import dataclass


@dataclass(slots=True, init=True, hash=True)
class Point:
    x: int
    y: int

def print_field(f: List[List[int]]) -> None:
    nll = max(max(f, key=lambda l: max(l))) // 10 + 1 # + 1 done by , in print
    for line in f:
        for i, c in enumerate(line):
            print(" " * (nll - len(str(c))), c, end="", sep=" " if i != 0 else "")
        print()

def get_neighbours(pos: Point, field) -> Generator[Point, None, None]:
    for dy in range(-1, 2, 2):
        for dx in range(-1, 2, 2):
            if pos.x + dx < 0 or pos.x + dx >= len(field) or pos.y + dx < 0 or pos.y + dx >= len(field):
                continue
            n = Point(pos.x + dx, pos.y + dy)
            if field[n.y][n.x] == 0:
                yield n

    
def get_reachable_positions(start: Point, field: List[List[int]], data: Dict[Point, List[Point]]) -> Dict[Point, List[Point]]:
    if not data:
        data[start] = [start]
    for neighbour in get_neighbours(start, field):
        if data.get(neighbour):
            continue
        data[neighbour] = data[start][:] + [neighbour]
        data |= get_reachable_positions(neighbour, field, data)
    return data
    

def solve(n: int, pairs: int):
    if n < 4 or int(n // 2 + 0.5) > pairs:
        print("N < 4 or too less pairs")
        return
        
    field = [[0 for _ in range(n)] for _ in range(n)]
    print(n, pairs, sep="\n")
    print_field(field)

    positions: Dict[int, Tuple[Point, Point]] = {}
    free_positions = [Point(x, y) for y in range(0, n) for x in range(0, n)]
    
    while len(positions) < pairs:
        pos1 = random.choice(free_positions)
        reachable_positions: Dict[Point, List[Point]] = get_reachable_positions(pos1, field, {})
        
        if not reachable_positions:
            # single closed field with no neighbours
            free_positions.remove(pos1)
            if not free_positions:
                print("FAILURE: Creating ARUKONE")
                solve(n, pairs)
            continue
        
        lpositions = list(reachable_positions)
        pos2 = lpositions[random.randint(0, len(lpositions))]
        
        for p in reachable_positions[pos2]:
            field[p.y][p.x] = len(positions)
        positions[pairs] = (pos1, pos2)
        
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
