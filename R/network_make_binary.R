#' @title Matrix convert to binary
#' @description This function takes a weighted adjacency matrix and separates
#' the components of the matrix into weights and presence/absence matrices
#' making use of a threshold.
#' @param adj_mat adjacency matrix to be converted.
#' @param threshold we recommended to use \code{\link{network_rescale}}
#' before using this function. Re-scaling will transform all values into a range [0,100].
#' @param print_message a logical value indicating whether or not a threshold message shall be printed.
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
#'  plot_network_dendrogram(ppcor_rescaled)
#'  plot_network_dendrogram(ppcor_binary)
#' @export

network_make_binary <- function (adj_mat, threshold, print_message = TRUE){
  
  if (print_message)
    message("network_make_binary() applies ", threshold, " as cut-off threshold to transform the input weighted adjacency matrix into a binary adjacency matrix.")
  
  binary <- apply(adj_mat, FUN = function(x) ifelse(x>threshold, yes = 1, no = 0), MARGIN = c(1,2))
  
  return(binary)
}