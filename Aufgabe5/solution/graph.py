from typing import Dict, Generic, List, Tuple, TypeVar

T = TypeVar("T")
V = TypeVar("V")

INF = float("inf")

class Node(Generic[T, V]):

    __slots__ = ["id", "value", "destinations"]    
    
    def __init__(self, id: T, value: V):
        self.id: T = id
        self.value: V = value
        self.destinations: Dict[Node, int] = {}
    
    def __repr__(self) -> str:
        return "<Node id=%r, value=%r; endpoints=%i>" % (self.id, self.value, len(self.destinations))
    
    def add_destination(self, destination: "Node", weight: int = 1, override: bool = True) -> None:
        if override and self.destinations.get(destination) is None:
            self.destinations[destination] = weight

    def remove_destination(self, destination: "Node") -> int:
        return self.destinations.pop(destination, 0)

    def get_destinations(self) -> List["Node[T, V]"]:
        return list(self.destinations.keys())

    

class Graph(Generic[T, V]):

    __slots__ = ["nodes"]
    
    def __init__(self, *node_list: Node[T, V]):
        self.nodes = {x.id: x for x in node_list}

    def add_node(self, node: Node[T, V]) -> None:
        self.nodes[node.id] = node
    
    def remove_node(self, node: Node[T, V]) -> None:
        self.nodes.pop(node.id)

    def add_edge(self, start: Node[T, V], end: Node[T, V], weight: int = 1):
        start.add_destination(end, weight)
    
    def remove_edge(self, start: Node[T, V], end: Node[T, V]) -> int:
        return start.remove_destination(end)

    def find_shortest_path(self, start: Node[T, V], target: Node[T, V]) -> Tuple[List[Node[T, V]], float]:
        distances: Dict[Node[T, V], float] = {
            start: 0
        }
        previous_nodes = {}        
        reachable: List[Node[T, V]] = []

        # initialize dijkstra's informations 
        for neighbour in start.destinations.keys():
            if neighbour != start:
                reachable.append(neighbour)
                distances[neighbour] = start.destinations[neighbour]
                previous_nodes[neighbour] = start
        
        # generate distances
        while reachable:
            current_node = min(reachable, key=lambda k: distances[k])
            reachable.remove(current_node)

            for neighbour in current_node.destinations.keys():
                if distances.get(neighbour, INF) == INF: 
                    # new node found
                    reachable.append(neighbour)

                if distances.get(neighbour, INF) > distances[current_node] + current_node.destinations[neighbour]:
                    # shorter path to this neighbour found
                    previous_nodes[neighbour] = current_node  # keep previous node for path retracing 
                    distances[neighbour] = distances[current_node] + current_node.destinations[neighbour]
                    
        # retrace path
        path = [target]

        while previous_nodes[path[0]] != start:
            path.insert(0, previous_nodes[path[0]])

        path.insert(0, start)
        
        return path, distances[target]
    
