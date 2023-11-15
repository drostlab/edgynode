#Calculate eigenvectors as centrality measure

summary_centrality_node_eigenvalues <- function(adj){
  igmat <- convert_adj_to_igraph(adj)
  igraph::eigen_centrality(igmat)$vector
}
