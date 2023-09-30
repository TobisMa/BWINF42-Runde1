import os
import sys
from typing import Dict, List, Optional, Tuple
from graph import Graph, Knot


WALL = "#"
WAY = "."
START = "A"
END = "B"

UP = "v"
DOWN = "^"
LEFT = "<"
RIGHT = ">"
SWITCH_UP = "U"
SWITCH_DOWN = "D"

DIRECTION_MAP: Dict[Tuple[int, int, int], str] = {
    (1, 0, 0): RIGHT,
    (-1, 0, 0): LEFT,
    (0, 1, 0): UP,
    (0, -1, 0): DOWN,
    (0, 0, 1): SWITCH_DOWN,
    (0, 0, -1): SWITCH_UP
}


def add_connections(g: Graph[Tuple[int, int, int], str]):
    for k in g.knots.values():
        (x, y, z), value = k.id, k.value
        if value == "#":
            continue

        for i in range(-1, 2, 2):
            tpos = (x + i, y, z)
            tknot = g.knots.get(tpos)
            if tknot and tknot.value != WALL:
                g.add_edge(k, tknot, 1)

            tpos = (x, y + i, z)
            tknot = g.knots.get(tpos)
            if tknot and tknot.value != WALL:
                g.add_edge(k, tknot, 1)

            tpos = (x, y, z + i)
            tknot = g.knots.get(tpos)
            if tknot and tknot.value != WALL:
                g.add_edge(k, tknot, 3) 


def parse_input(filename: str) -> Tuple[Graph, Knot, Knot, Tuple[int, int, int]]:
    g: Graph[Tuple[int, int, int], str] = Graph()
    floor = -1
    y = 0
    start = None,
    end = None
    prev_empty_line = True
    with open(filename, 'r') as f:
        dimensions = list(map(int, f.readline().split(" ")))

        for line in f:
            line = line.strip(" \n")
            if not line:
                # new floor
                y = 0
                prev_empty_line = True
                continue
            
            # increase floor count when a new floor starts instead of when a floor ends to prevent miscounting
            if prev_empty_line:
                floor += 1

            prev_empty_line = False
            
            # read in floor line
            for x, char in enumerate(line):
                pos = x, y, floor

                knot = Knot(pos, char)
                
                if char == START:
                    start = knot
                    knot.value = WAY
                
                if char == END:
                    end = knot
                    knot.value = WAY
                    
                g.add_knot(knot)

            y += 1
    
    # ensure the right floor count
    floor += 1
    
    dimensions = [dimensions[1], dimensions[0], floor]
    add_connections(g)

    return g, start, end, dimensions  # type: ignore

    
def find_direction(path: List[Knot[Tuple[int, int, int], str]], k: Knot[Tuple[int, int, int], str]):
    index = path.index(k)    
    if index == len(path) - 1:
        return END
    
    next_knot = path[index + 1]
    
    pos = k.id
    next_pos = next_knot.id
    
    diff = (
        next_pos[0] - pos[0],
        next_pos[1] - pos[1],
        next_pos[2] - pos[2]
    )

    return DIRECTION_MAP[diff]
    
    


def visualize(file_object, g: Graph[Tuple[int, int, int], str], path: List[Knot[Tuple[int, int, int], str]], dimensions: Tuple[int, int, int]):
    for floor in range(dimensions[2]):
        for height in range(dimensions[1]):
            # construct line
            line = ""
            for width in range(dimensions[0]):
                pos = (width, height, floor)
                k = g.knots[pos]

                # check if has direction and use according char
                char = k.value
                if k in path:
                    char = find_direction(path, k)
                    
                line += char

            print(line, flush=True)  # move into stream
        print()

    

def main(*input_files):
    files = list(input_files)
    
    if not files:
        files.append(input("Input file path: "))
        
    for file in files:        
        if not os.access(file, os.R_OK):
            print("Cannot open input file %r" % file)
        print("Parsing File %r" % file)
        graph, start, end, dimension = parse_input(file)

        path, distance = graph.dijkstra(start, end)

        print("Path length: %i" % distance)
        visualize(sys.stdout, graph, path, dimension)
        print()
    

    

if __name__ == "__main__":
    main(*sys.argv[1:])
