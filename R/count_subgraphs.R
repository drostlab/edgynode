# Function to count the number of subgraphs of a given size in a graph
count_subgraphs <- function(graph, subgraph_size) {
  num_subgraphs <- 0
  
  # Get the number of vertices in the graph
  num_vertices <- igraph::vcount(graph)
  
  # Iterate over all combinations of vertices
  for (i in 1:num_vertices) {
    for (j in i:num_vertices) {
      # Check if the subgraph induced by vertices i and j has the desired size
      if (subgraph_size == 2) {
        num_subgraphs <- num_subgraphs + 1
      } else if (subgraph_size > 2) {
        neighbors_i <- igraph::neighbors(graph, i)
        neighbors_j <- igraph::neighbors(graph, j)
        common_neighbors <- intersect(neighbors_i, neighbors_j)
        
        for (k in common_neighbors) {
          subgraph <- igraph::induced_subgraph(graph, c(i, j, k))
          if (igraph::vcount(subgraph) == subgraph_size) {
            num_subgraphs <- num_subgraphs + 1
          }
        }
      }
    }
  }
  
  return(num_subgraphs)
}