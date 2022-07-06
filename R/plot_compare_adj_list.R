plot_compare_adj_list <- function(df){
  ggplot2::ggplot(df, ggplot2::aes(x = metric, y = distance, fill = metric)) +
    ggplot2::theme_minimal() +
    ggplot2::geom_violin() +
    ggplot2::geom_point(
      size = 1,
      position = ggplot2::position_jitterdodge(jitter.width = 0.75),
      alpha = 1000 / nrow(df)
    )
}
