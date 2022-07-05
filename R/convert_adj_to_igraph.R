convert_adj_to_igraph <- function(adj){
  assert_known_standard(adj)
  if(attr(adj, "known_binary")){
    weighted <- NULL
  }else{
    weighted <- TRUE
  }
  igraph::graph_from_adjacency_matrix(
    adjmatrix = adj,
    weighted = weighted
  )
}
