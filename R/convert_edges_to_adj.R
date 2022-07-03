#' @title Convert an edge list to an adjacency matrix
#' @description This function takes an edge list and
#' transforms it into an adjacency matrix
#' @param edges the edge list data frame, with 2 or 3 columns
#' (from, to, value)
#' @author Ilias Moutsopoulos
#' @export

convert_edges_to_adj <- function(edges){
  edges <- edges_to_edges(edges)
  rows <- unique(edges$from)
  cols <- unique(edges$to)
  adj <- matrix(
    data = 0,
    nrow = length(rows),
    ncol = length(cols),
    dimnames = list(rows, cols)
  )
  # use apply and parallelise this
  for(k in seq_len(nrow(edges))){
    i <- match(edges$from[k], rows)
    j <- match(edges$to[k], cols)
    adj[i, j] <- edges$value[k]
  }
  adj
}
