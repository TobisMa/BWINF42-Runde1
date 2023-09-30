import os
import sys
from typing import List, Tuple
from graph import Graph, Knot


WALL = "#"
WAY = "."
START = "A"
END = "B"

UP = "^"
DOWN = "v"
LEFT = "<"
RIGHT = ">"
SWITCH_UP = "U"
SWITCH_DOWN = "D"


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
    
    if not prev_empty_line:
        # ensure the right floor count
        floor += 1
    
    dimensions.append(max(0, floor - 1))
    add_connections(g)

    return g, start, end, dimensions  # type: ignore


def visualize(g: Graph[Tuple[int, int, int], str], path: List[Knot[Tuple[int, int, int], str]], dimensions: Tuple[int, int, int]):
    print('\n'.join(str(k.id) for k in path))
    print(dimensions)


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
        print(distance)
        visualize(graph, path, dimension)

    

if __name__ == "__main__":
    main(*sys.argv[1:])
