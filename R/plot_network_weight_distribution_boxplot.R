#' @title Plot the weight distribution as boxplot
#' @description This function takes a weighted adjacency matrix and plots
#' the weight distribution for all edges of a gene.
#' @param adj_mat a weighted adjacency matrix.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param threshold a integer value between [0, 100] indicating where to draw the red vertical line.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples
#' # path to PPCOR output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#' # import PPCOR output into adjacency matrix
#' ppcor_parsed <- ppcor(ppcor_output)
#' # rescaling weighted adjacency matrix to range [0,100]
#' ppcor_rescaled <- network_rescale(ppcor_parsed)
#' # visualize weight distributions over all edges for each gene
#' plot_network_weight_distribution_boxplot(ppcor_rescaled, threshold = 70)
#' @export

plot_network_weight_distribution_boxplot <-
  function (adj_mat,
            xlab = "Edge weight",
            ylab = "Gene name",
            threshold) {
    adj_mat_long <-
      tidyr::pivot_longer(tibble::as_tibble(adj_mat), cols = 1:ncol(adj_mat))
    
    p <-
      ggplot2::ggplot(adj_mat_long, ggplot2::aes(x = value, y = name, colour = value)) + 
      ggplot2::geom_point(ggplot2::aes(y = name, color = value), size = .5, alpha = 0.8) +
      ggplot2::geom_boxplot(alpha = 0.5) +
      ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::geom_vline(
        xintercept = threshold,
        color = "red",
        size = 1.5,
        alpha = 0.3
      ) +
      ggplot2::scale_colour_continuous(high = "#132B43", low = "#56B1F7")
    
    return(p)
  }