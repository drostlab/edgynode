#' @title Convert a numeric matrix to binary
#' @description This function takes an adjacency matrix as input and
#' converts it to a binary one
#' @param adj the adjacency matrix
#' @param threshold the threshold used for binarisation; default is \code{0.7}
#' @author Ilias Moutsopoulos
#' @export

make_binary <- function(adj, threshold = 0.7){
  check_adjacency_error(adj)
  if(!attr(adj, "known_binary")){
    attr(adj, "known_binary") <- TRUE
    adj[] <- adj >= threshold
  }
  adj
}
