# -*- encoding: utf-8 -*-
import os


def inp():
    return int(input("Bild number: "))

    
def main():
    n = inp()
    
    with open(os.path.join(os.path.dirname(__file__), f"bild{n:0>2}", f"bild{n:0>2}.ppm"), "r") as f:
        f.seek(0)
        format = f.readline()
        width, height = list(map(int, f.readline().split(" ")))
        numbersize = f.readline()
        data = f.read()

    # print(f"{width=}; {height=}; {format=}; {numbersize=}")

    data = data.replace("\n", " ")
    while "  " in data:
        data = data.replace("  ", " ")
    
    picture = []
    cw = 0
    pixel = 0
    # print(data.split(" "))
    for color in data.split(" "):
        if not color:
            continue
        if cw == 0 and pixel == 0:
            picture.append([])
        if pixel == 0:
            picture[-1].append([])
        
        # print(picture)
        picture[-1][-1].append(int(color))
        
        pixel = (pixel + 1) % 3
        if pixel == 0:
            cw += 1
            cw %= width
    
    # print(picture)
        
    # reading message
    message = ""
    x = 0
    y = 0
    while (picture[y][x][1] != 0 or picture[y][x][2] != 0):
        pixel = picture[y][x]
        message += chr(pixel[0])
        # print(f"{pixel=}; {message=}")
        nx = (x + pixel[1]) % width
        ny = (y + pixel[2]) % height
        x = nx
        y = ny
        # print(f"{nx=}; {ny=}")

    print("Message:", message + chr(picture[y][x][0])) 
        

if __name__ == "__main__":
    main()
    
