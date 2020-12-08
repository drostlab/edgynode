#' @title Matrix convert to binary
#' @description This function takes a weighted adjacency matrix and separates
#' the components of the matrix into weights and presence/absence matrices
#' making use of a threshold.
#' @param adj_mat adjacency matrix to be converted.
#' @param threshold
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples

#' @export

network_make_binary <- function (adj_mat, threshold = 0.5){
  
  weights <- adj_mat
  
  binary <- apply(adj_mat, FUN = function(x) ifelse(x>threshold, yes = 1, no = 0), MARGIN = c(1,2))
  
  return(binary)
}