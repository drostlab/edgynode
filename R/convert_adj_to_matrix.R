#' @title Convert an adjacency matrix to plain matrix
#' @description This function takes an adjacency matrix and
#' transforms it into a plain matrix
#' @param adj the adjacency matrix
#' @author Ilias Moutsopoulos and Sergio Vasquez
#' @export

convert_adj_to_matrix <- function(adj){
  assert_adjacency(adj)
  x <- adj[]
  attr(x, "known_symmetric") <- NULL
  attr(x, "known_binary") <- NULL
  x
}
