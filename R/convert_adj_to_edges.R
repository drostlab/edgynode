#' @title Convert an adjacency matrix to an edge list
#' @description This function takes an adjacency matrix and
#' transforms it into an edge list
#' @param adj the adjacency matrix
#' @author Ilias Moutsopoulos
#' @export

convert_adj_to_edges <- function(adj){
  add_names_to_matrix(adj)
  data.frame(
    from = rep(rownames(adj), times = ncol(adj)),
    to = rep(colnames(adj), each = nrow(adj)),
    value = as.vector(adj)
  ) |>
    dplyr::filter(value != 0)
}
