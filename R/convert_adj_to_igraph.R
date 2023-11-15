#' @title Convert an adjacency matrix to igraph object
#' @description This function takes an adjacency matrix and
#' transforms it into an igraph object
#' @param adj the adjacency matrix
#' @author Ilias Moutsopoulos and Sergio Vasquez
#' @export

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
