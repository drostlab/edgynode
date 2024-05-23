#' Normalized shortest path kernels
#'
#' Once we find the shortest path between all pairs of nodes, 
#' we can use this information to iterate over all pairs of vertices 
#' in both graphs, and for each pair, the difference between their 
#' shortest path lengths is computed. The exponential function 
#' exp(-abs(sp1[i, j] - sp2[k, l])) is used to measure similarity, 
#' with smaller differences contributing more to the kernel value.
#' The normalization factors norm1 and norm2 are computed as the 
#' sum of the exponentials of the absolute differences between 
#' shortest path lengths. The final kernel value is normalized by 
#' dividing by the square root of the product of these normalization 
#' factors.
#' 
#' This function works with igraph objects of binarized matrices,
#' an example starting from unformatted weighted adjacency 
#' matrices is given. The function make_binary() requires a
#' threshold. 0.01 was chosen, modify if needed.
#'
#' @param graph1 first network in edge list format to compare
#' @param graph2 second network in edge list format to compare
#'   
#' @return  normalized_kernel_value, A similarity score
#' 
#' @example
#' # Create two example graphs
#' g1 <- make_binary(make_adjacency(adjacency_matrix_genie3_10), threshold = 0.01)
#' g1 <- graph_from_adjacency_matrix(g1)
#' g2 <- make_binary(make_adjacency(adjacency_matrix_test_3), threshold = 0.01)
#' g2 <- graph_from_adjacency_matrix(g2)
#'
#' # Compare the normalized shortest path kernels
#' kernel_value <- normalized_shortest_path_kernel(g1, g2)

normalized_shortest_path_kernel <- function(graph1, graph2) {
  # Calculate all-pairs shortest paths for both graphs
  sp1 <- igraph::distances(graph1)
  sp2 <- igraph::distances(graph2)
  
  # Initialize kernel value
  kernel_value <- 0
  
  # Iterate over all pairs of shortest paths and compute the kernel value
  for (i in seq_len(vcount(graph1))) {
    for (j in seq_len(vcount(graph1))) {
      if (i != j) {
        for (k in seq_len(vcount(graph2))) {
          for (l in seq_len(vcount(graph2))) {
            if (k != l) {
              if (sp1[i, j] != Inf && sp2[k, l] != Inf) {
                kernel_value <- kernel_value + exp(-abs(sp1[i, j] - sp2[k, l]))
              }
            }
          }
        }
      }
    }
  }
  
  # Normalize the kernel value
  norm1 <- sum(exp(-abs(outer(sp1, sp1, "-"))), na.rm = TRUE)
  norm2 <- sum(exp(-abs(outer(sp2, sp2, "-"))), na.rm = TRUE)
  
  normalized_kernel_value <- kernel_value / sqrt(norm1 * norm2)
  
  return(normalized_kernel_value)
}
