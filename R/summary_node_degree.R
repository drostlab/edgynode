summary_node_degree <- function(adj){
  check_known_symmetric_error(adj)
  rowSums(adj)
}
