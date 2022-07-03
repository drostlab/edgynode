#' Calculate graph distance based on DeltaCon
#'
#' Computes the distance between two graphs using the DeltaCon method.
#' @references D. Koutra, J. T. Vogelstein, and C. Faloutsos: DeltaCon: A Principled Massive-Graph Similarity Function. SIAM 2013: 162–170.
#' @references D. Koutra, T. Ke, U. Kang, D. H. Chau, H. K. Pao, C. Faloutsos: Unifying Guilt-by-Association Approaches: Theorems and Fast Algorithms. ECML/PKDD (2) 2011: 245-260
#'
#' @param adj1,adj2 the adjacency matrices
#' @param .MAX_POWER maximum power for matrix inversion
#' @return DeltaCon score between adj1 and adj2 (a scalar)
#' @export

metric_delta_con <- function(adj1, adj2, .MAX_POWER = 10){
  degrees1 <- summary_node_degree(adj1)
  degrees2 <- summary_node_degree(adj2)
  S1 <- summary_delta_con_similarity(adj1, degrees1, .MAX_POWER = .MAX_POWER)
  S2 <- summary_delta_con_similarity(adj2, degrees2, .MAX_POWER = .MAX_POWER)
  philentropy::matusita(S1, S2, testNA = FALSE)
}
