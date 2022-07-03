summary_node_degree <- function(adj){
  assert_known_symmetric(adj)
  rowSums(adj)
}
