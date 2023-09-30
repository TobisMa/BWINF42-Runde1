from typing import Any, Dict, Generic, List, Optional, Tuple, TypeVar

from torch import ne

T = TypeVar("T")
V = TypeVar("V")

INF = float("inf")

class Knot(Generic[T, V]):

    __slots__ = ["id", "value", "destinations"]    
    
    def __init__(self, id: T, value: Optional[V]):
        self.id: T = id
        self.value: Optional[V] = value
        self.destinations: Dict[Knot, int] = {}
    
    def __repr__(self) -> str:
        return "<Knot id=%r, value=%r; endpoints=%i>" % (self.id, self.value, len(self.destinations))
    
    def add_destination(self, destination: "Knot", weight: int = 1, override: bool = True) -> None:
        if override and self.destinations.get(destination) is None:
            self.destinations[destination] = weight

    def remove_destination(self, destination: "Knot") -> int:
        return self.destinations.pop(destination, 0)

    def get_destinations(self) -> List["Knot[T]"]:
        return list(self.destinations.keys())

    

class Graph(Generic[T, V]):

    __slots__ = ["knots"]
    
    def __init__(self, *knot_list: Knot[T, V]):
        self.knots = {x.id: x for x in knot_list}

    def add_knot(self, knot: Knot[T, V]) -> None:
        self.knots[knot.id] = knot
    
    def remove_knot(self, knot: Knot[T, V]) -> None:
        self.knots.pop

    def add_edge(self, start: Knot[T, V], end: Knot[T, V], weight: int = 1):
        start.add_destination(end, weight)
    
    def remove_edge(self, start: Knot[T, V], end: Knot[T, V]) -> int:
        return start.remove_destination(end)

    def dijkstra(self, start: Knot[T, V], target: Knot[T, V]) -> Tuple[List[Knot[T, V]], float]:
        distances: Dict[Knot[T, V], float] = {
            start: 0
        }
        previous_knot = {}
        
        reachable: List[Knot[T, V]] = []
        for neighbor in start.destinations.keys():
            if neighbor != start:
                reachable.append(neighbor)
                distances[neighbor] = start.destinations[neighbor]
                previous_knot[neighbor] = start
        
        # generate distances
        while reachable:
            current_knot = min(reachable, key=lambda k: distances[k])
            reachable.remove(current_knot)

            for neighbor in current_knot.destinations.keys():
                if distances.get(neighbor, INF) == INF: 
                    reachable.append(neighbor)

                if distances.get(neighbor, INF) > distances[current_knot] + current_knot.destinations[neighbor]:
                    previous_knot[neighbor] = current_knot
                    distances[neighbor] = distances[current_knot] + current_knot.destinations[neighbor]
                    
        # retrace path
        path = [target]

        while previous_knot[path[0]] != start:
            path.insert(0, previous_knot[path[0]])

        path.insert(0, start)
        
        return path, distances[target]
    
