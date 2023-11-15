#' Harmonic centrality of vertices
#'
#' The harmonic centrality of a vertex is the mean inverse distance to all other
#' vertices. The inverse distance to an unreachable vertex is considered to be zero.
#'
#' The `cutoff` argument can be used to restrict the calculation to paths
#' of length `cutoff` or smaller only; this can be used for larger graphs
#' to speed up the calculation. If `cutoff` is negative (which is the
#' default), then the function calculates the exact harmonic centrality scores.
#'
#' @param graph The graph to analyze.
#' @param vids The vertices for which harmonic centrality will be calculated.
#' @param mode Character string, defining the types of the paths used for
#'   measuring the distance in directed graphs. \dQuote{out} follows paths along
#'   the edge directions only, \dQuote{in} traverses the edges in reverse, while
#'   \dQuote{all} ignores edge directions. This argument is ignored for undirected
#'   graphs.
#' @param normalized Logical scalar, whether to calculate the normalized
#'   harmonic centrality. If true, the result is the mean inverse path length to
#'   other vertices, i.e. it is normalized by the number of vertices minus one.
#'   If false, the result is the sum of inverse path lengths to other vertices.
#' @param weights Optional positive weight vector for calculating weighted
#'   harmonic centrality. If the graph has a `weight` edge attribute, then
#'   this is used by default. Weights are used for calculating weighted shortest
#'   paths, so they are interpreted as distances.
#' @param cutoff The maximum path length to consider when calculating the
#'   harmonic centrality. There is no such limit when the cutoff is negative. Note that
#'   zero cutoff means that only paths of at most length 0 are considered.
#' @return Numeric vector with the harmonic centrality scores of all the vertices in
#'   `v`.
#' @seealso [betweenness()], [closeness()]
#' @references M. Marchiori and V. Latora, Harmony in the small-world,
#' *Physica A* 285, pp. 539-546 (2000).
#' @family centrality
#' @export
#' @keywords graphs
#' @examples
#'
#' g <- make_ring(10)
#' g2 <- make_star(10)
#' harmonic_centrality(g)
#' harmonic_centrality(g2, mode = "in")
#' harmonic_centrality(g2, mode = "out")
#' harmonic_centrality(g %du% make_full_graph(5), mode = "all")
#'
summary_centrality_harmonic <- function(graph, nodes = colnames(graph),mode = "out",weights = NULL,
                                        normalized = FALSE,
                                        cutoff = -1){
  igmat <- convert_adj_to_igraph(graph[nodes, nodes])
  igraph::harmonic_centrality(igmat,mode=mode,weights=weights,normalized=normalized,cutoff=cutoff)
}
