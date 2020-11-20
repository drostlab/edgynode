#' @title Function to plot a dendrogram based on gene interaction
#' @description This function takes an adjacency matrix representing a
#' gene regulatory network and builds a dendrogram based on
#' the stength of gene interactions
#' @param adj_mat an adjacency matrix to plot the dendrogram from.
#' @param dist_method method to be used to calculate distances
#' Default is \code{euclidean}.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @export
#' @examples
#' # path to PIDC output file
#' pidc_output <- system.file('beeline_examples/PIDC/outFile.txt', package = 'scNetworkR')
#' # parsing the output to an adjacency matrix
#' pidc_parsed <- pidc(pidc_output)
#' # rescaling the matrix
#' rescaled <- network_rescale(pidc_parsed)
#' # Visualize result
#' network_plot_dendrogram(rescaled)

network_plot_dendrogram <- function(adj_mat, dist_method = "euclidean") {

  # Plotting a dendrogram
  plot(hclust(stats::dist(t(adj_mat[2:nrow(adj_mat), 2:ncol(adj_mat)]))))

}
