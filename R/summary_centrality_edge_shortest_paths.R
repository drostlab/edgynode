#' @title Function to calculate betweeness in a network
#' @description This function an adjacency matrix as required by edgynode
#' @param adj, an adjacency matrix with adjacency attributes
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples
#' # if the matrix is not yet standard:
#' summary_centrality_edge_shortest_paths(make_standard(make_adjacency(adj)))
#' @export

summary_centrality_edge_shortest_paths <- function(adj){
  igmat <- convert_adj_to_igraph(adj)
  igraph::shortest.paths(igmat)
}
