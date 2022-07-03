#Function to calculate the closeness of edges and nodes

centrality_closeness <- function(adj_mat){
  #First convert from matrix to igraph object
  adj_mat[is.na(adj_mat)] <- 0
  adj_mat <- as.matrix(adj_mat)
  
  if(all(adj_mat %in% 0:1)){
    adj_mat <- igraph::graph.adjacency(adj_mat)
  }
  else {adj_mat <- igraph::graph_from_adjacency_matrix(adj_mat, weighted = T)}
  
  node_closeness <- igraph::closeness(adj_mat)
  return(node_closeness)
}