#' @title Check if a matrix is binary
#' @description This function takes an adjacency matrix as input and
#' checks if it is binary
#' @param adj the adjacency matrix
#' @author Ilias Moutsopoulos
#' @export

is_binary <- function(adj){
  assert_adjacency(adj)
  if(attr(adj, "known_binary")){
    return(TRUE)
  }else{
    return(identical(sum(abs(adj)) - sum(adj == 1), 0))
  }
}
