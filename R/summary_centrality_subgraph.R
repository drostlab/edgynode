#' Find subgraph centrality scores of network positions
#'
#' Subgraph centrality of a vertex measures the number of subgraphs a vertex
#' participates in, weighting them according to their size.
#'
#' The subgraph centrality of a vertex is defined as the number of closed loops
#' originating at the vertex, where longer loops are exponentially
#' downweighted.
#'
#' Currently the calculation is performed by explicitly calculating all
#' eigenvalues and eigenvectors of the adjacency matrix of the graph. This
#' effectively means that the measure can only be calculated for small graphs.
#'
#' @param graph The input graph assuming it is undirected
#' @param diag Boolean scalar, whether to include the diagonal of the adjacency
#'   matrix in the analysis. Giving `FALSE` here effectively eliminates the
#'   loops edges from the graph before the calculation.
#' @return A numeric vector, the subgraph centrality scores of the vertices.
#' @author Tobias Woertwein based on Gabor Csardi based on the Matlab
#' code by Ernesto Estrada
#' @references Ernesto Estrada, Juan A. Rodriguez-Velazquez: Subgraph
#' centrality in Complex Networks. *Physical Review E* 71, 056103 (2005).
#' @family centrality
#' @export
#' @keywords graphs
summary_centrality_subgraph <- function(graph,diag = FALSE){
  A <-graph
  if (!diag) {
    diag(A) <- 0
  }
  eig <- eigen(A)
  res <- as.vector(eig$vectors^2 %*% exp(eig$values))
  names(res) <-colnames(A)
  res
}
