import pandas as pd


path = r"Aufgabe5\data\tour1.txt"

df = pd.read_csv(path,skiprows=1,names=["Ort","Jahr","Essentiell?","Distanz von Start"])
tour = df.values.tolist()


print(df)


def find_subtours(tour):
    subtours = []
    open_stops = {}
    for i,stop in enumerate(tour):
        if stop[0] in open_stops:
            subtours += list(range(open_stops[stop[0]][1]+1,i+1))
            open_stops.clear()
        elif stop[2] == "X":
            open_stops.clear()
            open_stops[stop[0]] = (stop[3],i)
        else:
            open_stops[stop[0]] = stop[3]
    
    return subtours


def remove_stops(tour: list,stop_indices):
    for i in reversed(stop_indices):
        del tour[i]
    return tour


stops = find_subtours(tour)
new_tour = remove_stops(tour,stops)
print(pd.DataFrame(new_tour))