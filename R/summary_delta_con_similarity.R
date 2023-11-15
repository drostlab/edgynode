#' @title Calculate DeltaCon similarity
#' @description
#' Computes the similarity matrix of an adjacency based on the DeltaCon method.
#' Adapted from previous implementation by Baoxu(Dash) Shi, Data Sciense Group,
#' University of Notre Dame
#' @references D. Koutra, J. T. Vogelstein, and C. Faloutsos: DeltaCon:
#' A Principled Massive-Graph Similarity Function. SIAM 2013: 162-170.
#' @references D. Koutra, T. Ke, U. Kang, D. H. Chau, H. K. Pao, C. Faloutsos:
#' Unifying Guilt-by-Association Approaches: Theorems and Fast Algorithms.
#' ECML/PKDD (2) 2011: 245-260
#' @param adj the adjacency matrix
#' @param degrees the vector of node degrees
#' @param .MAX_POWER maximum power for matrix inversion
#' @param debug if TRUE, the function will gives you the time it spent on each step
#' @return The similarity matrix S as described in the DeltaCon method
#' @export

summary_delta_con_similarity <- function(
    adj,
    degrees,
    .MAX_POWER = 10,
    debug = FALSE
){
  output_time <- function(debug, tim, s) {
    if(debug) {
      print(paste(s, "user time:", tim[1], "system time:", tim[2], "elapsed time:", tim[3]))
    }
  }

  assert_adjacency(adj)
  A <- convert_adj_to_matrix(adj)
  nnodes <- nrow(A)

  # Identity matrix
  I = NULL
  tim <- system.time(
    {
      I <- diag(nrow = nnodes)
    })
  output_time(debug, tim, "Create identity matrix")

  # Diagonal matrix of node degrees
  D = NULL
  tim <- system.time(
    {
      D <- diag(degrees)
    })
  output_time(debug, tim, "Create degree diagonal matrix")

  # Compute about-half homophily factor to guarantee covergence
  c1 = sum(D) + 2
  c2 = sum(D ^ 2) - 1
  h_h = sqrt((-c1 + sqrt(c1 ^ 2 + 4 * c2)) / (8 * c2))

  # Compute constant ah and ch
  ah = 4 * h_h ^ 2 / (1 - 4 * h_h ^ 2)
  ch = 2 * h_h / (1 - 4 * h_h ^ 2)

  # Initialise M
  M = NULL
  tim <-system.time({
    M = ch * A  - ah * D
  })
  output_time(debug, tim, "Initialize Invert matrix")

  # Calculate inverse of M
  inv_ = I
  mat_ = M
  pow = 1
  tim <- system.time({
    while(max(mat_) > 1e-09 && pow < .MAX_POWER) {
      inv_ = inv_ + mat_
      mat_ = mat_ %*% M
      pow = pow + 1
    }
  })
  output_time(debug, tim, "Invert M")

  inv_ * 0.01
}
