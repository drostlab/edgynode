#' Find Bonacich alpha centrality scores of network positions
#'
#' `alpha_centrality()` calculates the alpha centrality of some (or all)
#' vertices in a graph.
#'
#' The alpha centrality measure can be considered as a generalization of
#' eigenvector centrality to directed graphs. It was proposed by Bonacich in
#' 2001 (see reference below).
#'
#' The alpha centrality of the vertices in a graph is defined as the solution
#' of the following matrix equation: \deqn{x=\alpha A^T x+e,}{x=alpha t(A)x+e,}
#' where \eqn{A}{A} is the (not necessarily symmetric) adjacency matrix of the
#' graph, \eqn{e}{e} is the vector of exogenous sources of status of the
#' vertices and \eqn{\alpha}{alpha} is the relative importance of the
#' endogenous versus exogenous factors.
#'
#' @aliases alpha.centrality
#' @param graph The input matrix
#' @param nodes List of Nodenames as a subselection
#' @param alpha Parameter specifying the relative importance of endogenous
#'   versus exogenous factors in the determination of centrality. See details
#'   below.
#' @param loops Whether to eliminate loop edges from the graph before the
#'   calculation.
#' @param exo The exogenous factors, in most cases this is either a constant --
#'   the same factor for every node, or a vector giving the factor for every
#'   vertex. Note that too long vectors will be truncated and too short vectors
#'   will be replicated to match the number of vertices.
#' @param weights A character scalar that gives the name of the edge attribute
#'   to use in the adjacency matrix. If it is `NULL`, then the
#'   \sQuote{weight} edge attribute of the graph is used, if there is one.
#'   Otherwise, or if it is `NA`, then the calculation uses the standard
#'   adjacency matrix.
#' @param tol Tolerance for near-singularities during matrix inversion, see
#'   [solve()].
#' @param sparse Logical scalar, whether to use sparse matrices for the
#'   calculation. The \sQuote{Matrix} package is required for sparse matrix
#'   support
#' @return A numeric vector contaning the centrality scores for the selected
#'   vertices.
#' @section Warning: Singular adjacency matrices cause problems for this
#' algorithm, the routine may fail is certain cases.
#' @author Gabor Csardi \email{csardi.gabor@@gmail.com}
#' @seealso [eigen_centrality()] and [power_centrality()]
#' @references Bonacich, P. and Lloyd, P. (2001). ``Eigenvector-like
#' measures of centrality for asymmetric relations'' *Social Networks*,
#' 23, 191-201.
#' @family centrality
#' @export
#' @keywords graphs
#' @examples
summary_centrality_alpha <- function(graph, nodes = colnames(graph), alpha = 1,
                                     loops = FALSE, exo = 1, weights = NULL,
                                     tol = 1e-7, sparse = TRUE){
  igmat <- convert_adj_to_igraph(graph[nodes, nodes])
  igraph::alpha_centrality(igmat, alpha = alpha,
                                  loops = loops, exo = exo, weights = weights,
                                  tol =tol, sparse = sparse)
}
