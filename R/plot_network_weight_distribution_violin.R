#' @title Plot the weight distribution as violin plot
#' @description This function takes a weighted adjacency matrix and plots
#' the weight distribution for all edges of a gene.
#' @param adj_mat a weighted adjacency matrix.
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
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples
#' # path to PPCOR output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#' # import PPCOR output into adjacency matrix
#' ppcor_parsed <- ppcor(ppcor_output)
#' # rescaling weighted adjacency matrix to range [0,100]
#' ppcor_rescaled <- network_rescale(ppcor_parsed)
#' # visualize weight distributions over all edges for each gene
#' plot_network_weight_distribution_violin(ppcor_rescaled, threshold = 70)
#' @export

plot_network_weight_distribution_violin <-
  function (adj_mat,
            xlab = "Edge weight",
            ylab = "Gene name",
            threshold) {
    adj_mat_long <-
      tidyr::pivot_longer(tibble::as_tibble(adj_mat), cols = 1:ncol(adj_mat))
    
    value <- name <- NULL
    
    if (threshold == "median") {
      threshold <- round(stats::median(adj_mat) , 2)
    }
    
    p <-
      ggplot2::ggplot(adj_mat_long, ggplot2::aes(x = value, y = name, colour = value)) +
      ggplot2::geom_point(
        size = 0.1,
        alpha = 0.4,
        colour = "black",
        position = ggplot2::position_jitter(width = 0, height = 0.28)
      ) +
      ggplot2::geom_violin(colour = "darkgray") +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
      ggplot2::geom_vline(
        xintercept = threshold,
        color = "red",
        size = 0.6,
        alpha = 0.3
      ) +
      ggplot2::theme(
        panel.background = ggplot2::element_rect(fill = NA),
        axis.title       = ggplot2::element_text(size = 16),
        axis.text.y      = ggplot2::element_text(size = 4),
        axis.text.x      = ggplot2::element_text(size = 16)
      )
    
    
    return(p)
  }