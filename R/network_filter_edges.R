#' @title Matrix filter edges
#' @description This function takes a weighted adjacency matrix filters out
#' the unsignificant edges
#' @param adj_mat adjacency matrix to be converted.
#' @param threshold if not specified, the function will filter based on the 
#' disparity measure (Neal, Z.P. (2022). backbone: An R package to extract 
#' network backbones) (recommended). If you want to specify threshold, we recommended to use \code{\link{network_rescale}}
#' before using this function. Re-scaling will transform all values into a range [0,100].
#' The threshold can either be a numeric balue in the interval [0,100] or a character string
#' specifying the following methods for automatically determining the threshold based on the input data:
#' @param print_message a logical value indicating whether or not a threshold message shall be printed.
#' @param p_value a value between setting the treshold for hypothesis testing for the backbone filtering.
#' @author Nikola Kalábová, Sergio Vasquez and Hajk-Georg Drost
#' @examples
#'  # path to PPCOR output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#'  # import PPCOR specific output
#' ppcor_parsed <- ppcor(ppcor_output)
#'  # rescale the matrix
#' ppcor_rescaled <- network_rescale(ppcor_parsed)
#'  # make the binary matrix
#' ppcor_binary <- network_make_binary(ppcor_rescaled)
#'  # to see the differences you may use:
#'  plot_network_dendrogram(ppcor_rescaled)
#'  plot_network_dendrogram(ppcor_binary)
#' @export

network_filter_edges <-
  function (adj_mat, threshold = NULL, print_message = TRUE, p_value = 0.1) {
    
    
    if (is.numeric(threshold)) {
      if (!dplyr::between(threshold, 0, 100)) {
        stop("Please provide a threshold value between [0,100].", call. = FALSE)
      }
    }
    if (is.numeric(p_value)) {
      if (!dplyr::between(p_value, 0, 1)) {
        stop("Please provide a p-value value between [0,1].", call. = FALSE)
      }
    }
    if (!is.numeric(p_value)) {
        stop("Please provide a numerical p-value value between [0,1].", call. = FALSE)
    }
    
    if (print_message) {
      if (is.numeric(threshold)) {
        message(
          "network_make_binary() applies [",
          threshold,
          "] as cut-off threshold to transform the input weighted adjacency matrix into a binary adjacency matrix."
        )
      }
      
      if (is.null(threshold)) {
        message(
          "Adjacency matrix will be created by computing the backbone of the graph."
        )}
      
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
    
    if  (is.null(threshold)) {
      net <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "directed", weighted = TRUE)
      bb <- backbone::disparity(net, alpha = p_value, narrative = TRUE)
      binary <- as.matrix(igraph::as_adjacency_matrix(bb))
      binary <- adj_mat * (binary == 1)
    }
    
    return(binary)
  }