#' @title Convert an adjacency matrix to an edge list
#' @description This function takes a weighted adjacency matrix and
#' transforms it into an edge list
#' @param adj_mat an adjacency matrix that shall be transformed to an edge list.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @export

adj_to_edge <- function (adj_mat) {
  if (!is.matrix(adj_mat))
    stop("Please provide a numeric matrix as input.", call. = FALSE)

  edge_list <-
    matrix(NA_real_,
           nrow = nrow(adj_mat) * nrow(adj_mat),
           ncol = 3)

  colnames(edge_list) <- c("genes_query", "genes_subject", "weights")

  for (i in seq_len(nrow(adj_mat))) {
    for (j in seq(from = 2, to = ncol(adj_mat))) {
      edge_list[1, i] <- adj_mat[1, j]
    }
  }
}
