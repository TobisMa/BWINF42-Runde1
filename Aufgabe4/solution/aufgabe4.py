from itertools import combinations, permutations
import os
import sys
from typing import Iterable, List, Tuple

ON = "On"
OFF = "Off"

LIGHT_STATES = ["OFF", "ON"]

LIGHT_SOURCE = "Q"
LIGHT_OUTPUT = "L"

BRICKS = {
    ("W", "W"): lambda l, r: (not (l and r), not (l and r)),
    ("B", "B"): lambda l, r: (l, r),
    ("R", "r"): lambda l, r: (not l, not l),
    ("r", "R"): lambda l, r: (not r, not r)
}


def tokenize_input(file: str) -> Tuple[List[bool], List[bool], List[List[str]]]:
    with open(file) as f:
        lines = list(f.readlines())
        
    _, first, *other = lines
    
    # getting a bool map of where the light sources Q1, Q2, ... are
    lights = [x.startswith(LIGHT_SOURCE) for x in filter(bool, first.split(" "))]
    
    # getting a bool map of where the light outputs L1, L2, ... are
    lamps = [x.startswith(LIGHT_OUTPUT) for x in filter(bool, other[-1].split(" "))]
    
    # converting each line below (line 2 and below) into their respective ascii token of W, B, R, r, X, L1, L2, L...
    tokens = [list(filter(bool, map(lambda x: x.strip(), line.split(" ")))) for line in other]

    return lights, lamps, tokens


def initial_light_states(light_map: List[bool]) -> Iterable[bool]:
    light_count = sum(light_map)
    for combination in range(int(2 ** light_count)):
        extra_count = -1
        light_states = []

        for x in range(len(light_map)):
            if light_map[x]:
                extra_count += 1
                if combination & (1 << extra_count) == (1 << extra_count):
                    light_states.append(True)
                    continue

            light_states.append(False)
            
        yield light_states

def translate_layer(lights: List[bool], layer: List[str]) -> List[bool]:
    prev = None
    nlights = []
    hadBrick = False
    for i, sym in enumerate(layer, start=0):
        if sym == "X":
            nlights.append(False)
        elif sym.startswith(LIGHT_OUTPUT):
            nlights.append(lights[i])
        
        elif prev is not None and not hadBrick:
            brick = (prev, sym)
            
            f = BRICKS.get(brick)
            if f:
                nlights.extend(f(lights[i-1], lights[i]))
                hadBrick = True
        
        else:
            hadBrick = False
        prev = sym

    return nlights



def print_results(results):
    for result in results:
        initial_light_state, light_map, lights, lamp_map = result
        output = ""
        count = 0
        for state, l in zip(initial_light_state, light_map):
            if l:
                count += 1
                output += ("Q%i: %s\n" % (count, LIGHT_STATES[state]))
                
        output += "\n"
        for state, l in zip(lights, lamp_map):
            if l:
                count += 1
                output += ("L%i: %s\n" % (count, LIGHT_STATES[state]))
        print(output[:-1])
        print("------------\n")
        


def solve(file: str):
    light_map, lamp_map, layers = tokenize_input(file)
    results = []
    for initial_light_state in initial_light_states(light_map):
        lights = initial_light_state
        for layer in layers:
            print(lights)
            lights = translate_layer(lights, layer)
            
        print("==========")
            
        results.append((initial_light_state, light_map, lights, lamp_map))
    
    print_results(results)
    

def main(*files: str):
    if not files:
        file = input("Filepath (relative to %s or absolute): " % (os.getcwd()))
        solve(file)
    
    else:
        for file in files:
            solve(file)
    
    exit(0)


if __name__ == "__main__":
    main(*sys.argv[1:])
