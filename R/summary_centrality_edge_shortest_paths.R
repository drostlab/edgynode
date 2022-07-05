#Function to calculate the closeness of edges and nodes

summary_centrality_edge_shortest_paths <- function(adj){
  igmat <- convert_adj_to_igraph(adj)
  igraph::shortest.paths(igmat)
}
