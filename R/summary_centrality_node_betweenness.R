#Function to calculate the in-betweenness of edges and nodes

summary_centrality_node_betweenness <- function(adj){
  igmat <- convert_adj_to_igraph(adj)
  igraph::betweenness(igmat)
}
