import os
import sys
from typing import Dict, List, Tuple

from graph import Graph, Node

# input chars (spaces cannot be used)
WALL = "#"
WAY = "."
START = "A"
END = "B"

# output chars for the path
UP = "v"
DOWN = "^"
LEFT = "<"
RIGHT = ">"
SWITCH_UP = "U"      # going floor down in a multilayered Bugwarts
SWITCH_DOWN = "D"    # going floor up in a multilayered Bugwarts
SWITCH_FLOOR = "!"   # generally for switching between floors; only used if there are exact two floors

# for which position change which output char should be used
DIRECTION_MAP: Dict[Tuple[int, int, int], str] = {
    (1, 0, 0): RIGHT,
    (-1, 0, 0): LEFT,
    (0, 1, 0): UP,
    (0, -1, 0): DOWN,
    (0, 0, 1): SWITCH_DOWN,
    (0, 0, -1): SWITCH_UP
}

# edge weights
TO_SAME_FLOOR = 1
TO_OTHER_FLOOR = 3


def add_connections(g: Graph[Tuple[int, int, int], str]):
    """
    Creates all edges between WAY nodes in the given graph

    Args:
        g (Graph[Tuple[int, int, int], str]): graph containing all the floor points and there point type (WAY or WALL)
    """

    for n in g.nodes.values():
        (x, y, z), value = n.id, n.value
        if value == "#":
            continue

        for i in range(-1, 2, 2):
            # adding left/right edge
            tpos = (x + i, y, z)
            tnode = g.nodes.get(tpos)
            if tnode and tnode.value != WALL:
                g.add_edge(n, tnode, TO_SAME_FLOOR)

            # adding up/down edge on floor plane (not floor switch)
            tpos = (x, y + i, z)
            tnode = g.nodes.get(tpos)
            if tnode and tnode.value != WALL:
                g.add_edge(n, tnode, TO_SAME_FLOOR)

            # adding up/down edge for floor switches
            tpos = (x, y, z + i)
            tnode = g.nodes.get(tpos)
            if tnode and tnode.value != WALL:
                g.add_edge(n, tnode, TO_OTHER_FLOOR)  # using 3 as weight 


def parse_input(filename: str) -> Tuple[Graph, Node, Node, Tuple[int, int, int]]:
    g: Graph[Tuple[int, int, int], str] = Graph()
    floor = -1
    y = 0
    x = 0
    start = None  # start node for the later dijkstra call
    end = None    # target node for the later dijkstra call 
    prev_empty_line = True

    with open(filename, 'r') as f:
        dimensions = list(map(int, f.readline().split(" ")))

        for line in f:
            line = line.rstrip(" \n")
            if not line:
                # new floor
                if y != dimensions[0]:
                    print("Incorrect dimensions")
                    sys.exit(-1)

                y = 0
                prev_empty_line = True
                continue
            
            # increase floor count when a new floor starts instead of when a floor ends to prevent miscounting
            if prev_empty_line:
                floor += 1

            prev_empty_line = False
            
            # read in floor line
            for x, char in enumerate(line):
                if char not in (WALL, WAY, START, END):
                    print("Unknown field character %r" % char)
                    sys.exit(-1)
                    
                pos = x, y, floor

                # keep WALL position for output later. WALL nodes won't have any connections later on
                node = Node(pos, char)
                
                if char == START:
                    start = node
                    node.value = WAY
                
                if char == END:
                    end = node
                    node.value = WAY
                    
                g.add_node(node)

            if x != dimensions[1] - 1:  # still unordered dimensions
                print(x)
                print("Incorrect dimensions in line %r" % line)
                sys.exit(-1)
            
            y += 1
    
    # ensure the right floor count
    floor += 1
    
    dimensions = [dimensions[1], dimensions[0], floor]  # reorder dimensions to have x, y, z format
    add_connections(g)

    return g, start, end, dimensions  # type: ignore

    
def find_direction(path: List[Node[Tuple[int, int, int], str]], k: Node[Tuple[int, int, int], str], multi_layerd: bool):
    """
    Returns the appropriate char for the direction Ron needs to go

    Args:
        path (List[Node[Tuple[int, int, int], str]]): the path which needs to be traveled
        k (Node[Tuple[int, int, int], str]): the current position on the path
        multi_layered (bool): Should the chars for multi-layered floors be used. `U` and `D` instead of `!`. 
            This clarifies the direction if the given Bugwarts has more than two floors 

    Returns:
        str: the character for that field
    """
    index = path.index(k)    
    if index == len(path) - 1:
        return END
    
    next_node = path[index + 1]
    
    pos = k.id
    next_pos = next_node.id
    
    diff = (
        next_pos[0] - pos[0],
        next_pos[1] - pos[1],
        next_pos[2] - pos[2]
    )

    d = DIRECTION_MAP[diff]
    if not multi_layerd and d in (SWITCH_DOWN, SWITCH_UP):
        return SWITCH_FLOOR
    return d
    

def visualize(g: Graph[Tuple[int, int, int], str], path: List[Node[Tuple[int, int, int], str]], dimensions: Tuple[int, int, int]):
    for floor in range(dimensions[2]):
        for height in range(dimensions[1]):
            # construct line
            line = ""
            for width in range(dimensions[0]):
                pos = (width, height, floor)
                k = g.nodes[pos]

                # check if has direction and use according char
                char = k.value
                if k in path:
                    char = find_direction(path, k, dimensions[2] >= 3)  # returns the character which should be used to describe the direction in which Ron has to go
                    
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

        # reading input
        graph, start, end, dimension = parse_input(file)

        # find path
        path, distance = graph.find_shortest_path(start, end)

        # converting result to human readable data
        print("Path length: %i" % distance)
        visualize(graph, path, dimension)
    

    

if __name__ == "__main__":
    main(*sys.argv[1:])
