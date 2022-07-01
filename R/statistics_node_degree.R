summary_single_node_degree <- function(adj){
  check_known_symmetric_error(adj)
  rowSums(adj)
}

summary_pair_node_degree_difference <- function(vec1, vec2){
  check_same_names_error(vec1, vec2)
  vec1 - vec2
}

metric_average_node_degree_difference <- function(adj1, adj2){
  vec1 <- summary_single_node_degree(adj1)
  vec2 <- summary_single_node_degree(adj2)
  vec <- summary_pair_node_degree_difference(vec1, vec2)
  mean(vec)
}
