#' @title Force a matrix into being symmetric
#' @description This function takes an adjacency matrix as input and forces it
#' into being symmetric.
#' @param adj the adjacency matrix
#' @param method the element-wise function to be applied to the matrix and
#' its transpose; default is \code{pmax}
#' @author Ilias Moutsopoulos
#' @export

make_symmetric <- function(adj, method = pmax){
  # note other methods may cause this function to no longer be orthogonal
  # with the scaling/binarisation; consider removing the parameter
  check_adjacency_error(adj)
  if(!attr(adj, "known_symmetric")){
    attr(adj, "known_symmetric") <- TRUE
    adj <- do.call(method, list(adj, t(adj)))
  }
  adj
}
