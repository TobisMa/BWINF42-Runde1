import pandas as pd





def print_subtour(subtour_indices, tour):
    subtour = [tour[idx][0] for idx in subtour_indices]
    print(" - ".join(subtour))

def find_subtours(tour):
    subtour_stops = []
    open_stops = {}
    for i, stop in enumerate(tour):
        if stop[0] in open_stops:
            subtour_indices = list(range(open_stops[stop[0]][1], i + 1))
            print_subtour(subtour_indices, tour)
            subtour_stops += subtour_indices[1:]
            open_stops.clear()
        elif stop[2] == "X":
            open_stops.clear()
            open_stops[stop[0]] = (stop[3], i)
        else:
            open_stops[stop[0]] = (stop[3], i)
    return subtour_stops


def remove_stops(tour: list,stop_indices):
    for i in reversed(stop_indices):
        del tour[i]
    return tour

def startStop(tour: list):
    start_tour = {}
    end_tour = {}

    for i, stop in enumerate(tour):
        start_tour[stop[0]] = i
        if stop[2] == "X":
            break

    for i, stop in enumerate(reversed(tour)):
        end_tour[stop[0]] = i
        if stop[2] == "X":
            break

    max_index = -1
    best_pair = []
    for stop in start_tour:
        if stop in end_tour and start_tour[stop] + end_tour[stop] > max_index:
            print(f"new best start: {stop}")
            best_pair = [start_tour[stop], len(tour)-end_tour[stop]]

    return tour[best_pair[0]:best_pair[1]]


def shorten_tour(tour: list):
    print("Looking for unnecessary subtours:")
    tour = remove_stops(tour,find_subtours(tour))
    print("Looking for a better start:")
    tour = startStop(tour)
    tour = [sublist[:-1] for sublist in tour]
    return tour


path = r"Aufgabe5\data\tour4.txt"

df = pd.read_csv(path,skiprows=1,names=["Ort","Jahr","Essentiell?","Distanz von Start"])
tour = df.values.tolist()
print(df)

improved_tour = shorten_tour(tour)

print(pd.DataFrame(improved_tour))