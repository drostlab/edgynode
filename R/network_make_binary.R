#' @title Matrix convert to binary
#' @description This function takes a weighted adjacency matrix and separates
#' the components of the matrix into weights and presence/absence matrices
#' making use of a threshold.
#' @param adj_mat adjacency matrix to be converted.
#' @param threshold we recommended to use \code{\link{network_rescale}}
#' before using this function. Re-scaling will transform all values into a range [0,100].
#' The threshold can either be a numeric balue in the interval [0,100] or a character string
#' specifying the following methods for automatically determining the threshold based on the input data:
#' \itemize{
#' \item \code{threshold = "median"}: compute the \code{\link{median}} over the entire input \code{adj_mat} and use this 
#' \code{median} value as threshold for defining all edge weights of a genes equal or below the \code{median}
#' value as \code{0} and all values above the \code{median} value as \code{1}.  
#' }
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

network_make_binary <-
  function (adj_mat, threshold, print_message = TRUE) {
    
    median_threshold <- round(stats::median(adj_mat), 2)
    
    
    if (is.numeric(threshold)) {
      if (!dplyr::between(threshold, 0, 100)) {
        stop("Please provide a threshold value between [0,100].", call. = FALSE)
      }
    }
    
    if (print_message) {
      if (is.numeric(threshold)) {
        message(
          "network_make_binary() applies [",
          threshold,
          "] as cut-off threshold to transform the input weighted adjacency matrix into a binary adjacency matrix."
        )
      }
      
      if (is.character(threshold)) {
        if (threshold == "median") {
          message(
            "network_make_binary() applies the median value over all values in the input matrix an uses [",
            median_threshold,
            "] as cut-off threshold to transform the input weighted adjacency matrix into a binary adjacency matrix."
          )
        }
        
      }
    }
    
    
    if (is.numeric(threshold)) {
      binary <-
        apply(
          adj_mat,
          FUN = function(x)
            ifelse(x > threshold, yes = 1, no = 0),
          MARGIN = c(1, 2)
        )
    }
    
    if (is.character(threshold)) {
      if (threshold == "median") {

        binary <-
          apply(
            adj_mat,
            FUN = function(x)
              ifelse(x > median_threshold, yes = 1, no = 0),
            MARGIN = c(1, 2)
          )
      }
    }
    
    return(binary)
  }