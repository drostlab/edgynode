
#' Kleinberg's hub and authority centrality scores.
#'
#' The hub scores of the vertices are defined as the principal eigenvector
#' of \eqn{A A^T}{A*t(A)}, where \eqn{A} is the adjacency matrix of the
#' graph.
#'
#' Similarly, the authority scores of the vertices are defined as the principal
#' eigenvector of \eqn{A^T A}{t(A)*A}, where \eqn{A} is the adjacency matrix of
#' the graph.
#'
#' For undirected matrices the adjacency matrix is symmetric and the hub
#' scores are the same as authority scores.
#'
#' @param graph The input graph.
#' @param mode Wether to calculate the hub or authority score (default:hub)
#' @param scale Logical scalar, whether to scale the result to have a maximum
#'   score of one. If no scaling is used then the result vector has unit length
#'   in the Euclidean norm.
#' @param weights Optional positive weight vector for calculating weighted
#'   scores. If the graph has a `weight` edge attribute, then this is used
#'   by default.
#'   This function interprets edge weights as connection strengths. In the
#'   random surfer model, an edge with a larger weight is more likely to be
#'   selected by the surfer.
#' @return A named list with members:
#'   \item{vector}{The hub or authority scores of the vertices.}
#'   \item{value}{The corresponding eigenvalue of the calculated
#'     principal eigenvector.}
#'   \item{options}{Some information about the ARPACK computation, it has
#'     the same members as the `options` member returned
#'     by [arpack()], see that for documentation.}
#' @seealso [eigen_centrality()] for eigenvector centrality,
#' [page_rank()] for the Page Rank scores. [arpack()] for
#' the underlining machinery of the computation.
#' @references J. Kleinberg. Authoritative sources in a hyperlinked
#' environment. *Proc. 9th ACM-SIAM Symposium on Discrete Algorithms*,
#' 1998. Extended version in *Journal of the ACM* 46(1999). Also appears
#' as IBM Research Report RJ 10076, May 1997.
#'
#' @export
kleinberg_score <- function(graph,mode="hub",scale = TRUE, weights = NULL){
  igmat <- convert_adj_to_igraph(graph)
  if(mode=="hub"){
    igraph::hub_score(igmat, scale = TRUE, weights = NULL)
  }else{
    igraph::authority_score(igmat, scale = TRUE, weights = NULL)
  }
}
