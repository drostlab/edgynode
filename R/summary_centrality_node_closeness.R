#Function to calculate the closeness of edges and nodes

summary_centrality_node_closeness <- function(adj){
  igmat <- convert_adj_to_igraph(adj)
  igraph::closeness(igmat)
}
