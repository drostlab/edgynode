plot_adjacency_weights <- function(adj, threshold){
  assert_adjacency(adj)
  edges <- convert_adj_to_edges(adj) |>
    poorman::mutate(node = factor(to, levels = rownames(adj)))
  p <- ggplot2::ggplot(edges) +
    ggplot2::theme_minimal() +
    ggplot2::geom_point(ggplot2::aes(x = node, y = value, colour = value)) +
    ggplot2::geom_hline(
      yintercept = threshold,
      color = "red",
      size = 1.5,
      alpha = 0.3
    ) +
    ggplot2::ylim(min(adj), max(adj)) +
    ggplot2::scale_colour_continuous(low = "#56B1F7", high = "#132B43") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  p
}
