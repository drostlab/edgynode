#' @title Generate correlation heatmap
#' @description This function takes a weighted adjacency matrix and generates 
#' a heatmap in which the highest values are moved towards the top left corner
#' to improve readability
#' @param adj_mat adjacency matrix to be converted.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples

#' @export

plot_network_weight_distribution <- function (adj_mat, xlab = "Edge weight", ylab = "Gene name", threshold = 70) {
  
  adj_mat_long <- tidyr::pivot_longer(tibble::as_tibble(adj_mat), cols = 1:ncol(adj_mat))
  
  p <- ggplot2::ggplot(adj_mat_long, ggplot2::aes(x = value, y = name, colour = value)) + ggplot2::geom_point() +
    ggplot2::xlab(xlab) + ggplot2::ylab(ylab) + ggplot2::geom_vline(xintercept = threshold, color="red", size=1.5, alpha=0.3)
  
  return(p)
}