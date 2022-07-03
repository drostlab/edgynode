convert_edges_to_edges <- function(edges){
  if(ncol(edges) == 2) edges[, 3] <- 1
  colnames(edges)[1:3] <- c("from", "to", "value")
  edges |> as.data.frame() |> dplyr::select(1:3)
}
