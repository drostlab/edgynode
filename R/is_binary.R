#' @title Check if a numeric matrix generated with \code{\link{make_adjacency}} and \code{\link{make_binary}} is binary
#' @description This function takes an adjacency matrix as input and
#' checks if it is binary
#' @param adj an adjacency matrix converted from a raw input matrix via \code{\link{make_adjacency}} and optionally binarised with \code{\link{make_binary}}. 
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
