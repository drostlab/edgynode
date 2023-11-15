#' @title Plot the weight distribution for all edges of a gene before and after filtering
#' @description This function takes a weighted adjacency matrix and plots
#' the weight distribution for all edges of a gene.
#' @param adj_mat unfiltered weighted adjacency matrix.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param threshold we recommended to use \code{\link{network_rescale}}
#' before using this function. Re-scaling will transform all values into a range [0,100].
#' The threshold can either be a numeric balue in the interval [0,100] or a character string
#' specifying the following methods for automatically determining the threshold based on the input data:
#' \itemize{
#' \item \code{threshold = "median"}: compute the \code{\link{median}} over the entire input \code{adj_mat} and use this 
#' \code{median} value as threshold for defining all edge weights of a genes equal or below the \code{median}
#' value as \code{0} and all values above the \code{median} value as \code{1}.  
#' }
#' The threshold value will then be drawn as vertical line in the plot.
#' @param threshold if not specified, the function will filter based on the 
#' disparity measure (Neal, Z.P. (2022). backbone: An R package to extract 
#' network backbones) (recommended). If you want to specify threshold, we recommended to use \code{\link{network_rescale}}
#' before using this function. Re-scaling will transform all values into a range [0,100].
#' The threshold can either be a numeric balue in the interval [0,100] or a character string
#' specifying the following methods for automatically determining the threshold based on the input data:
#' @param print_message a logical value indicating whether or not a threshold message shall be printed.
#' @param p_value a value between setting the treshold for hypothesis testing for the backbone filtering.
#' @param x_ticks number of y-axis ticks.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples
#' # path to PPCOR output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#' # import PPCOR output into adjacency matrix
#' ppcor_parsed <- ppcor(ppcor_output)
#' # rescaling weighted adjacency matrix to range [0,100]
#' ppcor_rescaled <- network_rescale(ppcor_parsed)
#' # visualize weight distributions over all edges for each gene
#' plot_network_weight_distribution(ppcor_rescaled, threshold = 70)
#' @export

plot_filtering_network_weight_distribution <-
  function (adj_mat,
            xlab = "Edge weight",
            ylab = "Gene name",
            threshold = NULL,
            x_ticks = 10,
            print_message = TRUE, 
            p_value = 0.1) {
    
    filtered <- network_filter_edges(adj_mat,threshold = threshold,print_message = print_message, p_value = p_value)
    p1 = plot_network_weight_distribution(adj_mat)
    p2 = plot_network_weight_distribution(filtered)
    cowplot::plot_grid(p1, p2, ncol = 2)
  }