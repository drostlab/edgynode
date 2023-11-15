#' @title Plot all the node weights of an adjacency
#' @param adj an \pkg{edgynode} adjacency.
#' @param threshold a numeric vector of length 1 or a named numeric vector of 
#' the same size as the number of nodes. The names of the vector must be the 
#' names of the nodes in the adjacency.
#' @return The plot as a ggplot object.
#' @examples
#' adj <- edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
#' plot_adjacency_weights(adj, threshold = 3)
#' plot_adjacency_weights(adj, threshold = c("N1" = 2, "N2" = 1, "N3" = 3))
#' @export
plot_adjacency_weights <- function(adj, threshold = NULL){
  assert_adjacency(adj)
  edges <- convert_adj_to_edges(adj) |>
    poorman::mutate(node = factor(to, levels = rownames(adj)))
  p <- ggplot2::ggplot(edges) +
    ggplot2::theme_minimal() +
    ggplot2::geom_point(ggplot2::aes(x = node, y = value, colour = value)) +
    ggplot2::ylim(min(adj), max(adj)) +
    ggplot2::scale_colour_continuous(low = "#56B1F7", high = "#132B43") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  if(!is.null(threshold)){
    if(length(threshold) == 1){
      p <- p +
        ggplot2::geom_hline(
          yintercept = threshold,
          color = "red",
          size = 1.5,
          alpha = 0.3
        )
    }else{
      p <- p +
        ggplot2::geom_line(
          data = data.frame(
            "node" = names(threshold),
            "dummy" = "x",
            "threshold" = threshold
          ),
          mapping = ggplot2::aes(x = node, y = threshold, group = dummy),
          color = "red",
          size = 1.5,
          alpha = 0.3
        )
    }
  }
  p
}
