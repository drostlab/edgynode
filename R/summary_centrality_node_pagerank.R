#Calculate PageRank as centrality measure

summary_centrality_node_pagerank <- function(adj){
  igmat <- convert_adj_to_igraph(adj)
  igraph::page_rank(igmat)$vector
}
