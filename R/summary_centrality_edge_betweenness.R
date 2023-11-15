summary_centrality_edge_betweenness <- function(adj){
  igmat <- convert_adj_to_igraph(adj)
  # this gives a vector corresponding to non-zero edges, need to convert to matrix
  edge_vec <- igraph::edge_betweenness(igmat)
  mat <- t(convert_adj_to_matrix(adj))
  mat[mat > 0] <- edge_vec
  t(mat)
}
