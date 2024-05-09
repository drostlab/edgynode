#' Graphlet kernel similarity score
#'
#' Graphlets are small, connected subgraphs of a larger graph. 
#' In graph theory, graphlets serve as building blocks for more 
#' complex networks. By comparing the occurrences and distributions
#' of graphlets in different graphs, one can quantify the structural 
#' similarity between the graphs.
#' The graphlet kernel computes a similarity score between two graphs
#' based on the number of common graphlets they share.
#'
#' @param net_1 first network in adjacency matrix format to compare
#' @param net_2 second network in adjacency matrix format to compare
#' @param size (n) of the graphlets to be compared, default = 3
#'   
#' @return A similarity score
#' @example # Generate two sample adjacency matrices
#' net_1 <- matrix(sample(0:1, 25, replace = TRUE), nrow = 5)
#' net_2 <- matrix(sample(0:1, 25, replace = TRUE), nrow = 5)
#'
#' #Compute similarity score using graphlet kernel
#' similarity_score <- compare_graphlet_kernel(net_1, net_2)
#' print(similarity_score)

compare_graphlet_kernel <- function(net_1, net_2, graphlet_size = 3) {
  # Convert matrices to graphs
  graph1 <- igraph::graph_from_adjacency_matrix(net_1, mode = "undirected")
  graph2 <- igraph::graph_from_adjacency_matrix(net_2, mode = "undirected")
  
  # Compute graphlet counts for both graphs
  graphlet_counts1 <- count_subgraphs(graph1, graphlet_size)
  graphlet_counts2 <- count_subgraphs(graph2, graphlet_size)
  
  # Compute similarity score using graphlet counts
  similarity_score <- sum(graphlet_counts1 * graphlet_counts2)
  
  return(similarity_score)
}