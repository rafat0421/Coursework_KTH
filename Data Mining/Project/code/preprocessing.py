import networkx as nx
import pandas as pd
from timeit import default_timer as timer

G = nx.Graph()
G = nx.read_gml('../data/power/power.gml', label='id')
start = timer()
nx.betweenness_centrality(G)
end = timer()
print(f'time elapsed -> {end - start}')



# df = pd.read_csv('../data/moreno_health/out.moreno_health_health', sep=' ', header=None, skiprows=1)
# df.columns = ["source", "target", "weight"]
# graph_type = nx.DiGraph()
# G = nx.from_pandas_edgelist(df, "source", "target", edge_attr="weight", create_using=graph_type)
# start = timer()
# nx.betweenness_centrality(G, weight='weight')
# end = timer()
# print(f'time elapsed -> {end - start}')
