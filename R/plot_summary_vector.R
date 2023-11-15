#' @title Plot vector summaries of an (or more) adjacency
#' @param x in the simplest case, a vector representing a summary of an adjacency 
#' matrix (for example generated with \code{\link{summary_node_degree}}). 
#' Alternatively, a named list of such vectors (names will be used for the plot legend).
#' @param individual if \code{TRUE} (default), plot each vector as a line.
#' If \code{FALSE}, plot a boxplot of all vectors at each node.
#' @param show_legend if \code{TRUE} (default), show the legend. 
#' Only applicable when \code{individual = TRUE}.
#' @return The plot as a ggplot object.
#' @examples
#' adj1 <- adj2 <- adj3 <- edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
#' adj2[1, 3] <- 5
#' adj3[1, 3] <- 0
#' adj1 <- edgynode::make_standard(adj1)
#' adj2 <- edgynode::make_standard(adj2)
#' adj3 <- edgynode::make_standard(adj3)
#' 
#' x <- list(
#'   "node_degree_adj1" = summary_node_degree(adj1),
#'   "node_degree_adj2" = summary_node_degree(adj2),
#'   "node_degree_adj3" = summary_node_degree(adj3)
#' )
#' 
#' plot_summary_vector(x)
#' plot_summary_vector(x, individual = FALSE)
#' @export
plot_summary_vector <- function(x, individual = TRUE, show_legend = TRUE){
  df <- collect_summary_vectors_to_df(x)
  p <- ggplot2::ggplot(df) + ggplot2::theme_minimal()
  if(individual){
    p <- p +
      ggplot2::geom_point(ggplot2::aes(x = xnum, y = value, colour = id)) +
      ggplot2::geom_line(ggplot2::aes(x = xnum, y = value, colour = id))
    if(!show_legend){
      p <- p + ggplot2::theme(legend.position = "none")
    }
  }else{
    p <- p +
      ggplot2::geom_boxplot(ggplot2::aes(x = name, y = value)) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  p + ggplot2::ylim(0, NA)
}
