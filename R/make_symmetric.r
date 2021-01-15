#' @title Force a matrix into being symmetric
#' @description This function takes and \code{adj_mat} as input and forces it into being symmetric.
#' @param adj_mat an adjacency matrix that shall be made symmetric.
#' @author lmshk
#' @export

network_make_symmetric <- function(adj_mat)
  pmax(adj_mat, t(adj_mat))
