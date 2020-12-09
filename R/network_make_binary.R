#' @title Matrix convert to binary
#' @description This function takes a weighted adjacency matrix and separates
#' the components of the matrix into weights and presence/absence matrices
#' making use of a threshold.
#' @param adj_mat adjacency matrix to be converted.
#' @param threshold it's recommended to use the network_rescale() function
#' before using this function. This will require a threshold with value 0-100.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples
#'  # path to PPCOR output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#'  # import PPCOR specific output
#' ppcor_parsed <- ppcor(ppcor_output)
#'  # rescale the matrix
#' ppcor_rescaled <- network_rescale(ppcor_parsed)
#'  # make the binary matrix
#' ppcor_binary <- network_make_binary(ppcor_rescaled, 70)
#'  # to see the differences you may use:
#'  network_plot_dendrogram(ppcor_rescaled)
#'  network_plot_dendrogram(ppcor_binary)
#' @export

network_make_binary <- function (adj_mat, threshold){
  
  weights <- adj_mat
  
  binary <- apply(adj_mat, FUN = function(x) ifelse(x>threshold, yes = 1, no = 0), MARGIN = c(1,2))
  
  return(binary)
}