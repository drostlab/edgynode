#' @title Check whether input matrix is an adjacency matrix
#' @description Imports a matrix and tests whether it is an adjacency in \pkg{edgynode} format.
#' @param x a \code{\link{matrix}} object.
#' @examples 
#'  
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
