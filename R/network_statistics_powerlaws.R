#' @title Calculate the component distribution from adjacency matrices
#' @description This function uses the \pkg{igraph} package to calculate
#' the component distribution from a adjacency matrix.
#' @param adj_mat a symmetrical adjacency matrix.
#' @param weighted This argument specifies whether to create a weighted graph from an adjacency matrix.
#' If it is NULL then an unweighted graph is created and the elements of the adjacency matrix gives the number
#' of edges between the vertices. If it is a character constant then for every non-zero matrix entry
#' an edge is created and the value of the entry is added as an edge attribute named by the weighted argument. If it is TRUE then a weighted graph is created and the name of the edge attribute will be weight. See also details below.
#' @param adjacency_mode a character value specifying how \code{igraph} should interpret the input matrices.
#' Options are:
#' \itemize{
#' \item If \code{weighted = NULL}:
#' \itemize{
#' \item \code{adjacency_mode = "directed"}: The graph will be directed and a matrix element gives the number of edges between two vertices.
#' \item \code{adjacency_mode = "undirected"}: This is exactly the same as max, for convenience. Note that it is not checked whether the matrix is symmetric (default).
#' \item \code{adjacency_mode = "upper"}: An undirected graph will be created, only the upper right triangle (including the diagonal) is used for the number of edges.
#' \item \code{adjacency_mode = "lower"}: An undirected graph will be created, only the lower left triangle (including the diagonal) is used for creating the edges.
#' \item \code{adjacency_mode = "max"}: An undirected graph will be created and max(A(i,j), A(j,i)) gives the number of edges.
#' \item \code{adjacency_mode = "min"}: undirected graph will be created with min(A(i,j), A(j,i)) edges between vertex i and j.
#' \item \code{adjacency_mode = "plus"}: undirected graph will be created with A(i,j)+A(j,i) edges between vertex i and j.
#' }
#'\item If the \code{weighted} argument is not \code{NULL} then the elements of the matrix give the weights of the edges (if they are not zero). The details depend on the value of the mode argument:
#'itemize{
#' \item \code{adjacency_mode = "directed"}: The graph will be directed and a matrix element gives the edge weights.
#' \item \code{adjacency_mode = "undirected"}: First we check that the matrix is symmetric. It is an error if not. Then only the upper triangle is used to create a weighted undirected graph (default).
#' \item \code{adjacency_mode = "upper"}: An undirected graph will be created, only the upper right triangle (including the diagonal) is used (for the edge weights).
#' \item \code{adjacency_mode = "lower"}: An undirected graph will be created, only the lower left triangle (including the diagonal) is used for creating the edges.
#' \item \code{adjacency_mode = "max"}: An undirected graph will be created and max(A(i,j), A(j,i)) gives the edge weights.
#' \item \code{adjacency_mode = "min"}: An undirected graph will be created, min(A(i,j), A(j,i)) gives the edge weights.
#' \item \code{adjacency_mode = "plus"}: An undirected graph will be created, A(i,j)+A(j,i) gives the edge weights.
#' }
#' }
#'
#' @param diag a logical value specifying whether to include the diagonal of the matrix in the calculation. If \code{diag = FALSE} then the diagonal first set to zero and then passed along the downstream functions.
#' @param add_colnames a character value specifying whether column names shall be added as vertex attributes. Options are:
#' \itemize{
#' \item \code{add_colnames = NULL} (default):  if present, column names are added as vertex attribute ‘name’.
#' \item \code{add_colnames = NA}: column names will not be added.
#' \item \code{add_colnames = ""}: If a character constant is specified then it gives the name of the vertex attribute to add.
#' }
#' @param add_rownames a character value specifying whether to add the row names as vertex attributes. Possible values the same as the previous argument. By default row names are not added. If ‘add.rownames’ and ‘add.colnames’ specify the same vertex attribute, then the former is ignored.
#' @param vertex_ids vertex ids for which the node degree will be calculated. Default is \code{\link[igraph]{V}}. See \code{\link[igraph]{degree}} for details.
#' @param degree_mode a string specifying the degree_mode passed on to \code{\link[igraph]{degree}}.
#' Options are:
#' \itemize{
#' \item \code{degree_mode = "out"}: for out-degree
#' \item \code{degree_mode = "in"}: for in-degree
#' \item \code{degree_mode = "total"}: for the sum of the two
#' \item \code{degree_mode = "all"}: is a synonym of \code{degree_mode = "total"}
#' }
#' For undirected graphs this argument is ignored.
#' @param loops logical value indicating whether the loop edges are also counted.
#' @param normalized logical value indicating whether to normalize the degree. If TRUE then the result is divided by n-1, where n is the number of vertices in the graph.
#' @param xmin a numeric scalar or \code{NULL}. The lower bound for fitting the power-law.
#' If \code{NULL}, the smallest value in x will be used for the ‘R.mle’ implementation, and its value will be automatically determined for the ‘plfit’ implementation.
#' This argument makes it possible to fit only the tail of the distribution. See \code{\link[igraph]{fit_power_law}} for details.
#' @param start a numeric scalar. The initial value of the exponent for the minimizing function, for the ‘R.mle’ implementation.
#' Usually, it is safe to leave this untouched.
#' @param force_continuous a logical scalar. Whether to force a continuous distribution for the ‘plfit’ implementation, even if the sample vector contains integer values only (by chance). If this argument is false, igraph will assume a continuous distribution if at least one sample is non-integer and assume a discrete distribution otherwise.
#' @param implementation a character scalar. Which implementation to use. See details below.
#' @param \dots additional arguments passed on to \code{\link[igraph]{graph_from_adjacency_matrix}}.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @export
#' @examples
#' # path to PPCOR output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'scNetworkR')
#' # import PPCOR output into adjacency matrix
#' ppcor_parsed <- PPCOR(ppcor_output)
#' # calculate network statistics
#' ppcor_statistics <- network_statistics_powerlaws(ppcor_parsed)
#' # look at results
#' ppcor_statistics
#'
network_statistics_powerlaws <- function(adj_mat,
                                         weighted = TRUE,
                                         adjacency_mode = "undirected",
                                         diag = TRUE,
                                         add_colnames = NULL,
                                         add_rownames = NA,
                                         vertex_ids = igraph::V(graph),
                                         degree_mode = c("all", "out", "in", "total"),
                                         loops = TRUE,
                                         normalized = FALSE,
                                         xmin = NULL,
                                         start = 2,
                                         force_continuous = FALSE,
                                         implementation = c("plfit", "R.mle"),
                                         ...) {
  # if (!isSymmetric(mat_adj))
  # stop(
  #   "Please provide a symmetric matrix as 'adj_mat' input for network_statistics().",
  #   call. = FALSE
  # )
  g <-
    igraph::graph.adjacency(
      as.matrix(adj_mat[, 2:ncol(adj_mat)]),
      weighted = weighted,
      mode = adjacency_mode,
      diag = diag,
      add.colnames = add_colnames,
      add.rownames = add_rownames,
      ...
    )

  g_degree <- igraph::degree(
    g,
    v = vertex_ids,
    mode = degree_mode,
    loops = loops,
    normalized = normalized
  )

  g_powerlaw <- igraph::fit_power_law(
    g_degree,
    xmin = xmin,
    start = start,
    force.continuous = force_continuous,
    implementation = implementation
  )

  return(g_powerlaw)
}
