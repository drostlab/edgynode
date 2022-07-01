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
#' # path to PPCOR output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#' # import PPCOR specific output
#' ppcor_parsed <- ppcor(ppcor_output)
#' # rescale matrix
#' ppcor_rescaled <- network_rescale(ppcor_parsed)
#' # plot dendrogram
#' plot_network_dendrogram(ppcor_rescaled)

plot_network_dendrogram <- function(adj_mat, dist_method = "euclidean") {

  # Plotting a dendrogram
  plot(stats::hclust(stats::dist(t(adj_mat[2:nrow(adj_mat), 2:ncol(adj_mat)]))))

}
