#' @export
summary_node_degree <- function(adj){
  assert_known_standard(adj)
  rowSums(adj)
}
