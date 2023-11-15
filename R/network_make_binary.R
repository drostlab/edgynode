#' @title Matrix convert to binary
#' @description This function takes a weighted adjacency matrix and created a 
#' binary adjacency matrix, filtering the unsignificant edges, if specified
#' @param adj_mat adjacency matrix to be converted.
#' @param threshold relevant only for not-prefiltered input matrix. If not 
#' specified, the function will filter based on the disparity measure (Neal, Z.P. (2022). 
#' backbone: An R package to extract network backbones) (recommended). If you 
#' want to specify threshold, we recommend to use \code{\link{network_rescale}}
#' before using this function. Re-scaling will transform all values into a range [0,100].
#' The threshold should be a numeric value in the interval [0,100]
#' @param print_message a logical value indicating whether or not a threshold message shall be printed.
#' @param filtered a logical value indicating whether the input adjacency network 
#' has been already prefiltered and thus no more edges should be removed
#' @author Sergio Vasquez, Hajk-Georg Drost and Nikola Kalábová
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

network_make_binary <-
  function (adj_mat, threshold = NULL, print_message = TRUE, filtered = FALSE) {
    
    if (print_message) {
      if (filtered == TRUE) {
        message(
          "Assigning every edge the value of 1"
        )
      }
    }
    
    if (filtered == FALSE) {
      mat <- network_filter_edges(adj_mat,threshold = threshold, print_message = print_message)
      
    }
    binary <- ifelse(mat != 0, 1, 0)
    return(binary)
  }