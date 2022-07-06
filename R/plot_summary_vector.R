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
