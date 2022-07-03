metric_average_node_degree_difference <- function(adj1, adj2){
  vec1 <- summary_single_node_degree(adj1)
  vec2 <- summary_single_node_degree(adj2)
  vec <- summary_pair_node_degree_difference(vec1, vec2)
  mean(vec)
}
