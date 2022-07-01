#' @title Convert an adjacency matrix to an edge list
#' @description This function takes an adjacency matrix and
#' transforms it into an edge list
#' @param adj the adjacency matrix
#' @author Ilias Moutsopoulos
#' @export

adj_to_edges <- function(adj){
  if(is.null(rownames(adj))) rownames(adj) <- paste0("R", seq_len(nrow(adj)))
  if(is.null(colnames(adj))) colnames(adj) <- paste0("T", seq_len(ncol(adj)))
  data.frame(
    from = rep(rownames(adj), times = ncol(adj)),
    to = rep(colnames(adj), each = nrow(adj)),
    value = as.vector(adj)
  )
}

#' @title Convert an edge list to an adjacency matrix
#' @description This function takes an edge list and
#' transforms it into an adjacency matrix
#' @param edges the edge list data frame, with 2 or 3 columns
#' (from, to, value)
#' @author Ilias Moutsopoulos
#' @export

edges_to_adj <- function(edges){
  edges <- edges_to_edges(edges)
  rows <- unique(edges$from)
  cols <- unique(edges$to)
  adj <- matrix(
    data = 0,
    nrow = length(rows),
    ncol = length(cols),
    dimnames = list(rows, cols)
  )
  for(k in seq_len(nrow(edges))){
    i <- match(edges$from[k], rows)
    j <- match(edges$to[k], cols)
    adj[i, j] <- edges$value[k]
  }
  adj
}

edges_to_edges <- function(edges){
  if(ncol(edges) == 2) edges[, 3] <- 1
  colnames(edges)[1:3] <- c("from", "to", "value")
  edges %>% as.data.frame() %>% dplyr::select(1:3)
}
