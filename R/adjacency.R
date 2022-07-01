make_adjacency <- function(x, known_symmetric = FALSE, known_binary = FALSE){
  if(is_adjacency(x)) stop("Matrix is already an adjacency")
  if(!is.matrix(x)) stop("Please supply a matrix")
  if(nrow(x) != ncol(x)) stop("Matrix must be square")
  if(is.null(rownames(x)) & is.null(colnames(x))){
    x <- add_names_to_matrix(x)
  }else if(is.null(rownames(x))){
    rownames(x) <- colnames(x)
  }else if(is.null(colnames(x))){
    colnames(x) <- rownames(x)
  }
  if(any(rownames(x) != colnames(x))) stop("Matrix row and column names must match")
  if(!is.numeric(x)) stop("Matrix must have numeric values")
  adj <- structure(x, class = c("adjacency", class(x)))
  attr(adj, "known_symmetric") <- known_symmetric
  attr(adj, "known_binary") <- known_binary
  adj
}

is_adjacency <- function(x){
  all(
    inherits(x, "adjacency"),
    is.matrix(x),
    nrow(x) == ncol(x),
    !(is.null(rownames(x)) | is.null(colnames(x))),
    all(rownames(x) == colnames(x)),
    is.numeric(x),
    !is.null(attr(x, "known_symmetric")),
    !is.null(attr(x, "known_binary"))
  )
}
