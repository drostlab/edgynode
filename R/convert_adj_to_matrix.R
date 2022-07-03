convert_adj_to_matrix <- function(adj){
  assert_adjacency(adj)
  x <- adj[]
  attr(x, "known_symmetric") <- NULL
  attr(x, "known_binary") <- NULL
  x
}
