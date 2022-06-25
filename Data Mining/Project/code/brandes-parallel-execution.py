import numpy as np
from queue import PriorityQueue
import networkx as nx
import pandas as pd
import math
import os
from time import sleep, perf_counter
import threading


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

def parallel_execution(number_of_thread, graph, num_of_vertices, vertex_per_thread):
    global start_from_vertex,end_to_vertex
    #G, input_matrix = read_graph('soc-sign-bitcoinotc.csv.gz')
    #graph = Graph(100, input_matrix)
    if number_of_thread == 0:
        start_from_vertex = 0
        end_to_vertex = start_from_vertex + vertex_per_thread
    if number_of_thread > 0:
        start_from_vertex = start_from_vertex + vertex_per_thread + 1
        end_to_vertex = start_from_vertex + vertex_per_thread
    e = graph.betweenness_centrality(graph.dijkstra, start_from_vertex, end_to_vertex)
    print(e)


class Graph:
    def __init__(self, num_of_vertices, adjacent_matrix, number_of_thread, vertex_per_thread):
        self.vertices = vertex_per_thread
        print(self.vertices)
        # Adjacent matrix is the weight matrix, for weighted graph, the elements should be weights between each 2 vertices, -100 for those who are not connceted
        # for unweighted graphs, the elements should be q if 2 vertices are connected, 0 if not
        self.edges = adjacent_matrix
        # Record vertices that have been visited
        self.visited = []

    def dijkstra(self, start_vertex, start_from,end_to):
        #start_vertex= 0
        # Initially, define the distance from start_vertex to any other vertices is infinite
        D = {v: float("inf") for v in range(start_from,end_to)}
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

            for neighbor in range(start_from,end_to):
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

    #def betweenness_centrality(self, func, start_from_vertex, end_to_vertex):
        # print('check 2:', self.vertices)
        # BC = {bc: 0 for bc in range(start_from_vertex,end_to_vertex)}
        # for s in range(self.vertices):
            # dependency = {delta: 0 for delta in range(start_from_vertex,end_to_vertex)}
            #print('check dep:',dependency)
            # D, previous_vertex, number_of_paths = func(s, start_from_vertex)
            # print('check_3:',previous_vertex)
            # D.pop(s)
            # D = sorted(D.items(), key=lambda d: d[1], reverse=True)
            
            # for w in D:
                # if w[1] != float("inf"):
                    # for v in previous_vertex[w[0]]:
                        # dependency[v] += (
                            # number_of_paths[v]
                            # / number_of_paths[w[0]]
                            # * (1 + dependency[w[0]])
                        # )
                    # BC[w[0]] += dependency[w[0]]
        # return BC
        
    def betweenness_centrality(self, func, start_from_vertex, end_to_vertex):
        BC = {bc: 0 for bc in range(start_from_vertex,end_to_vertex)}
        for s in range(start_from_vertex,end_to_vertex):
            dependency = {delta: 0 for delta in range(start_from_vertex,end_to_vertex)}
            D, previous_vertex, number_of_paths = func(s, start_from_vertex, end_to_vertex)
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
    input_matrix = np.asarray(input_matrix)
    #e = graph.betweenness_centrality(graph.dijkstra)
    threads = []
    number_of_thread = 5
    num_of_vertices = G.number_of_nodes()
    vertex_per_thread = math.floor(num_of_vertices/number_of_thread)
    graph = Graph(num_of_vertices, input_matrix, number_of_thread, vertex_per_thread)
    start_time = perf_counter()
    for k in range(number_of_thread):
        t = threading.Thread(target=parallel_execution, args=(k,graph,num_of_vertices,vertex_per_thread,))
        threads.append(t)
        t.start()
        t.join()
    #e = graph.BetweennessCentrality(graph.dijkstra)
    # e = graph.BetweennessCentrality(graph.BFS)
    #print(e)
    end_time = perf_counter()
    print(f'It took {end_time- start_time: 0.2f} second(s) to complete.')

