make_adjacency <- function(x, known_symmetric = FALSE, known_binary = FALSE){
  if(is_adjacency(x)) stop("Matrix is already an adjacency")
  if(!is.matrix(x)) stop("Please supply a matrix")
  if(nrow(x) != ncol(x)) stop("Matrix must be square")
  x <- add_names_to_matrix(x)
  if(any(rownames(x) != colnames(x))) stop("Matrix row and column names must match")
  if(!is.numeric(x)) stop("Matrix must have numeric values")
  adj <- structure(x, class = c("adjacency", class(x)))
  attr(adj, "known_symmetric") <- known_symmetric # Is it symmetric?
  attr(adj, "known_binary") <- known_binary # Is it binary?
  adj
}
