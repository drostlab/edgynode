#' @title Plot the weight distribution for all edges of a gene
#' @description This function takes a weighted adjacency matrix and plots
#' the weight distribution for all edges of a gene.
#' @param adj_mat a weighted adjacency matrix.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param threshold a numeric value in the interval [0,100] 
#' The threshold value will then be drawn as vertical line in the plot. Only relevant, if you provided treshold for the filtering.
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

plot_network_weight_distribution <-
  function (adj_mat,
            xlab = "Edge weight",
            ylab = "Gene name",
            threshold = NULL,
            x_ticks = 10) {
    
    adj_mat_long <-
      tidyr::pivot_longer(tibble::as_tibble(adj_mat), cols = 1:ncol(adj_mat))
    
    value <- name <- NULL
    
    if (is.numeric(threshold)) {
      p <- 
        ggplot2::ggplot(adj_mat_long, ggplot2::aes(x = value, y = name, colour = value)) + ggplot2::geom_point() +
        ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
        ggplot2::geom_vline(
          xintercept = threshold,
          color = "red",
          size = 1.5,
          alpha = 0.3
        ) + ggplot2::scale_colour_continuous(high = "#132B43", low = "#56B1F7")
    }
    
    else if (is.null(threshold)){
        p <- 
          ggplot2::ggplot(adj_mat_long, ggplot2::aes(x = value, y = name, colour = value)) + ggplot2::geom_point() +
          ggplot2::xlab(xlab) + ggplot2::ylab(ylab) +
          ggplot2::scale_colour_continuous(high = "#132B43", low = "#56B1F7") +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = x_ticks))
    }
    else {
        stop("Please provide a numeric treshold, or don't specify this atribute", call. = FALSE)
    }
    
    
    return(p)
  }