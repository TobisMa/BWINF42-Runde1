import pandas as pd


path = r"Aufgabe5\data\tour1.txt"

df = pd.read_csv(path,skiprows=1,names=["Ort","Jahr","Essentiell?","Distanz von Start"])
tour = df.values.tolist()


print(df)


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


subtour_stops = find_subtours(tour)
new_tour = remove_stops(tour,subtour_stops)
print(pd.DataFrame(new_tour))