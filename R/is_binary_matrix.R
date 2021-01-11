# Helper function to test whether or not an input matrix 
# is a binary adjacency matrix
is_binary_matrix <- function(adj_mat) {
  return(all(as.numeric(as.logical(adj_mat)) %in% 0:1))
}