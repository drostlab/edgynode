plot_network_degree_distribution_naive <- function (deg_mat){
  
  ggplot2::ggplot(deg_mat, ggplot2::aes(x = name, y = factor(degree, levels = sort(unique(as.character(degree)))), group = 1)) + ggplot2::geom_line()
  plot(sort(deg_mat), type = "b")
}