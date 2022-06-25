import numpy as np
from queue import PriorityQueue
import networkx as nx
import pandas as pd
import math
import os


# def read_graph(file='soc-sign-bitcoinotc.csv.gz'):
def read_graph(filename):
    if filename.endswith(".gml"):
        G = nx.read_gml(filename, label="id")
        adj_matrix = nx.adjacency_matrix(G)
        return G, adj_matrix

    df = pd.read_csv(filename, sep=" ", header=None, skiprows=1)
    df.columns = ["source", "target", "weight"]
    graph_type = nx.DiGraph()
    G = nx.from_pandas_edgelist(
        df, "source", "target", edge_attr="weight", create_using=graph_type
    )
    adj_matrix = nx.adjacency_matrix(G, weight="weight").todense()
    return G, adj_matrix


class Graph:
    def __init__(self, num_of_vertices, adjacent_matrix):
        self.vertices = num_of_vertices
        # Adjacent matrix is the weight matrix, for weighted graph, the elements should be weights between each 2 vertices, -100 for those who are not connceted
        # for unweighted graphs, the elements should be q if 2 vertices are connected, 0 if not
        self.edges = adjacent_matrix
        # Record vertices that have been visited
        self.visited = []

    def dijkstra(self, start_vertex):
        # Initially, define the distance from start_vertex to any other vertices is infinite
        D = {v: float("inf") for v in range(self.vertices)}
        # The distance from start_vertex to itself is 0
        D[start_vertex] = 0
        # pq is PriorityQueue
        pq = PriorityQueue()
        pq.put((0, start_vertex))
        # Record the predecessors for each vertex
        previous_vertex = {}
        number_of_paths = {start_vertex: 1}
        self.visited = []

        while not pq.empty():
            # Get the prioritized vertex, which has the shortest distance to the previous visited vertex
            (dist, current_vertex) = pq.get()
            # Mark vertices that have been visited
            self.visited.append(current_vertex)

            for neighbor in range(self.vertices):
                # Neighbors should be related to the current vertex
                if self.edges[current_vertex, neighbor] != 0:
                    # Vertices that have been visited should not be visited again
                    if neighbor not in self.visited:
                        # Update the shortest distance to the start_vertex(source vertex)
                        old_cost = D[neighbor]
                        new_cost = (
                            D[current_vertex] + self.edges[current_vertex, neighbor]
                        )
                        if new_cost < old_cost:
                            # Add the vertex to PriorityQueue
                            pq.put((new_cost, neighbor))
                            D[neighbor] = new_cost
                            previous_vertex[neighbor] = [current_vertex]
                            number_of_paths[neighbor] = number_of_paths[current_vertex]
                        if new_cost == old_cost:
                            if current_vertex not in previous_vertex[neighbor]:
                                previous_vertex[neighbor].append(current_vertex)
                            number_of_paths[neighbor] += number_of_paths[current_vertex]
        return D, previous_vertex, number_of_paths

    def bfs(self, start_vertex):
        # Initially, define the distance from start_vertex to any other vertices is infinite
        D = {v: float("inf") for v in range(self.vertices)}
        # The distance from start_vertex to itself is 0
        D[start_vertex] = 0
        # pq is PriorityQueue
        pq = PriorityQueue()
        pq.put((0, start_vertex))
        # Record the predecessors for each vertex
        previous_vertex = {v: [] for v in range(self.vertices)}
        number_of_paths = {v: 0 for v in range(self.vertices)}
        number_of_paths[start_vertex] = 1
        self.visited = []

        while not pq.empty():
            # Get the prioritized vertex, which has the shortest distance to the previous visited vertex
            (dist, current_vertex) = pq.get()
            # Mark vertices that have been visited
            self.visited.append(current_vertex)

            for neighbor in range(self.vertices):
                # Neighbors should be related to the current vertex
                if self.edges[current_vertex][neighbor] > 0:
                    # Neighbor vertx has been found for the first time?
                    if D[neighbor] == float("inf"):
                        # Add the vertex to PriorityQueue
                        D[neighbor] = D[current_vertex] + 1
                        pq.put((D[neighbor], neighbor))
                    if D[neighbor] == D[current_vertex] + 1:
                        number_of_paths[neighbor] += number_of_paths[current_vertex]
                        previous_vertex[neighbor].append(current_vertex)
        return D, previous_vertex, number_of_paths

    def betweenness_centrality(self, func):
        BC = {bc: 0 for bc in range(self.vertices)}
        for s in range(self.vertices):
            dependency = {delta: 0 for delta in range(self.vertices)}
            D, previous_vertex, number_of_paths = func(s)
            D.pop(s)
            D = sorted(D.items(), key=lambda d: d[1], reverse=True)
            for w in D:
                if w[1] != float("inf"):
                    for v in previous_vertex[w[0]]:
                        dependency[v] += (
                            number_of_paths[v]
                            / number_of_paths[w[0]]
                            * (1 + dependency[w[0]])
                        )
                    BC[w[0]] += dependency[w[0]]
        return BC


if __name__ == "__main__":
    G, input_matrix = read_graph("../data/moreno_health/out.moreno_health_health")
    graph = Graph(G.number_of_nodes(), input_matrix)
    input_matrix = np.asarray(input_matrix)
    e = graph.betweenness_centrality(graph.dijkstra)
    print(e)

