#' @title Check if a matrix is symmetric
#' @description This function takes an adjacency matrix as input and checks if
#' it is symmetric.
#' @param adj the adjacency matrix
#' @author Ilias Moutsopoulos
#' @export

is_symmetric <- function(adj){
  assert_adjacency(adj)
  if(attr(adj, "known_symmetric")){
    return(TRUE)
  }else{
    return(isSymmetric(adj))
  }
}
