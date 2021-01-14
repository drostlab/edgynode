#' @title Plot the naive degree distribution of a degree table
#' @description This function takes a degree table generated with
#' \code{\link{network_make_binary}} and \code{network_degree_distribution_naive} as input
#' and visualizes the corresponding naive degree distribution.
#' @param degree_tbl a degree table generated with
#' \code{\link{network_make_binary}} and \code{network_degree_distribution_naive}.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param y_ticks number of ticks on y-axis.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples
#' # path to PPCOR output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#' # import PPCOR output into adjacency matrix
#' ppcor_parsed <- ppcor(ppcor_output)
#' # rescaling matrix
#' ppcor_rescaled <- network_rescale(ppcor_parsed)
#' # make weighted adjacency matrix binary
#' ppcor_binary_adj_mat <- network_make_binary(ppcor_rescaled, threshold = 70)
#' # calculate network statistics
#' ppcor_statistics <- network_statistics_degree_distribution_naive(ppcor_binary_adj_mat)
#' # look at results
#' plot_network_degree_distribution_naive(ppcor_statistics)
#' @export


plot_network_degree_distribution_naive <-
  function (degree_tbl,
            xlab = "Gene Name",
            ylab = "Node Degree per Gene",
            y_ticks = 12) {
    
    node_degree_vctr <- degree_tbl$node_degree
    names(node_degree_vctr) <- degree_tbl$gene_name
    # sort the names by increasing node degree
    node_degree_vctr_names_sorted <- names(sort(node_degree_vctr))
    
    gene_name <- node_degree <- NULL
    p <-
      ggplot2::ggplot(degree_tbl, ggplot2::aes(
        x = factor(gene_name, levels = node_degree_vctr_names_sorted),
        y = node_degree,
        group = 1
      )) +
      ggplot2::geom_point(size = 6) +
      ggplot2::geom_line(size = 6, alpha = 0.4) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::theme(
        title            = ggplot2::element_text(size = 18, face = "bold"),
        legend.title     = ggplot2::element_text(size = 18, face = "bold"),
        legend.text      = ggplot2::element_text(size = 18, face = "bold"),
        axis.title       = ggplot2::element_text(size = 18, face = "bold"),
        axis.text.y      = ggplot2::element_text(size = 18, face = "bold"),
        axis.text.x      = ggplot2::element_text(size = 6, face = "bold"),
        panel.background = ggplot2::element_blank(),
        strip.text.x     = ggplot2::element_text(
          size           = 18,
          colour         = "black",
          face           = "bold"
        )
      ) + ggplot2::theme(axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 1,
        hjust = 1
      )) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = y_ticks))
    
    return (p)
  }
