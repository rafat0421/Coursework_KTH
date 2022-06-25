import numpy as np
import pandas as pd
import random as rnd
import networkx as nx
from itertools import count
from collections import deque
from heapq import heappush, heappop
from timeit import default_timer as timer


def get_graph_from_file(filename):
    if filename.endswith(".gml"):
        G = nx.read_gml(filename, label="id")
        return G

    df = pd.read_csv(filename, sep=" ", header=None, skiprows=1)
    df.columns = ["source", "target", "weight"]
    graph_type = nx.DiGraph()
    G = nx.from_pandas_edgelist(
        df, "source", "target", edge_attr="weight", create_using=graph_type
    )
    return G


def betweenness_centrality(G, k=None, normalized=True, weighted=None):
    betweenness = dict.fromkeys(G, 0.0)  # b[v]=0 for v in G
    if k is None:
        nodes = G
    else:
        nodes = rnd.sample(list(G.nodes()), k)

    for s in nodes:
        # single source shortest paths
        if weighted is None:  # use BFS
            S, P, sigma, _ = single_source_shortest_path_basic(G, s)
        else:  # use Dijkstra's algorithm
            S, P, sigma, _ = single_source_dijkstra_path_basic(G, s, weighted)
        # accumulation
        betweenness, _ = accumulate_basic(betweenness, S, P, sigma, s)
    # rescaling
    betweenness = rescale(betweenness, len(G), normalized=normalized, k=k)
    return betweenness


def accumulate_basic(betweenness, S, P, sigma, s):
    delta = dict.fromkeys(S, 0)
    while S:
        w = S.pop()
        coeff = (1 + delta[w]) / sigma[w]
        for v in P[w]:
            delta[v] += sigma[v] * coeff
        if w != s:
            betweenness[w] += delta[w]
    return betweenness, delta


def single_source_virtual_node_algorithm(G):
    # TODO
    pass


def single_source_shortest_path_basic(G, s):
    S = []
    P = {}
    for v in G:
        P[v] = []
    sigma = dict.fromkeys(G, 0.0)  # sigma[v]=0 for v in G
    D = {}
    sigma[s] = 1.0
    D[s] = 0
    Q = deque([s])
    while Q:  # use BFS to find shortest paths
        v = Q.popleft()
        S.append(v)
        Dv = D[v]
        sigmav = sigma[v]
        for w in G[v]:
            if w not in D:
                Q.append(w)
                D[w] = Dv + 1
            if D[w] == Dv + 1:  # this is a shortest path, count paths
                sigma[w] += sigmav
                P[w].append(v)  # predecessors
    return S, P, sigma, D


def single_source_dijkstra_path_basic(G, s, weight):
    weight = lambda u, v, data: data["weight"]
    S = []
    P = {}
    for v in G:
        P[v] = []
    sigma = dict.fromkeys(G, 0.0)  # sigma[v]=0 for v in G
    D = {}
    sigma[s] = 1.0
    push = heappush
    pop = heappop
    seen = {s: 0}
    c = count()
    Q = []  # use Q as heap with (distance,node id) tuples
    push(Q, (0, next(c), s, s))
    while Q:
        (dist, _, pred, v) = pop(Q)
        if v in D:
            continue  # already searched this node.
        sigma[v] += sigma[pred]  # count paths
        S.append(v)
        D[v] = dist
        for w, edgedata in G[v].items():
            vw_dist = dist + weight(v, w, edgedata)
            if w not in D and (w not in seen or vw_dist < seen[w]):
                seen[w] = vw_dist
                push(Q, (vw_dist, next(c), v, w))
                sigma[w] = 0.0
                P[w] = [v]
            elif vw_dist == seen[w]:  # handle equal paths
                sigma[w] += sigma[v]
                P[w].append(v)
    return S, P, sigma, D


def rescale(betweenness, n, normalized, k=None):
    if normalized:
        scale = 1 / (n * (n - 1))
    else:
        scale = None

    if scale is not None:
        if k is not None:
            scale = scale * n / k
        for v in betweenness:
            betweenness[v] *= scale

    return betweenness


def weight_function(G, weight):
    if callable(weight):
        return weight
    # If the weight keyword argument is not callable, we assume it is a
    # string representing the edge attribute containing the weight of
    # the edge.
    if G.is_multigraph():
        print("is_muligraph")
        return lambda u, v, d: min(attr.get(weight, 1) for attr in d.values())
    print("data.get(weight)")
    return lambda u, v, data: data.get(weight, 1)


if __name__ == "__main__":
    G_1 = get_graph_from_file("../data/power/power.gml")
    num_nodes_1 = G_1.number_of_nodes()
    G_2 = get_graph_from_file("../data/moreno_health/out.moreno_health_health")
    num_nodes_2 = G_2.number_of_nodes()

    b_1 = betweenness_centrality(G_1)
    b_2 = betweenness_centrality(G_2, weighted='weight')

    df_1 = pd.DataFrame(b_1,index=[0]).T
    df_1.to_excel('power.xlsx')

    df_2 = pd.DataFrame(b_2,index=[0]).T
    df_2.to_excel('health.xlsx')

    # rnd.seed(1000)
    # start = step = int(num_nodes_2 / 5)
    # for k in range(start, num_nodes_2 - step, step):
        # tot_time = 0.0
        # for i in range(5):
            # start_timer = timer()
            # b = betweenness_centrality(G_2, k=k, weighted="weight")
            # end_timer = timer()
            # elapsed_time = end_timer - start_timer
            # tot_time += elapsed_time
            # print(f"k: {k}, Elapsed time: {elapsed_time}")

        # print(f"Average time for k={k}: {tot_time/5}")

    # tot_time = 0.0
    # for i in range(5):
        # start_timer = timer()
        # b = betweenness_centrality(G_2, weighted="weight")
        # end_timer = timer()
        # elapsed_time = end_timer - start_timer
        # tot_time += elapsed_time
        # print(f"k: {num_nodes_1}, Elapsed time: {elapsed_time}")
    # print(f"Average time for k={num_nodes_1}: {tot_time/5}")
