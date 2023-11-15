#' @title Convert a raw input matrix into an \pkg{edgynode} adjacency matrix
#' @description Users may work with a vast variety of input matrices that
#' are derived from various gene regulatory network inference methods and tools
#' with various values and value ranges. This function aims to ensure that
#' such raw data is converted into a \pkg{edgynode} adjacency matrix format
#' and is the first conversion step when importing existing \code{raw}
#' adjacency matrices into the \pkg{edgynode} ecosystem.
#' @param x a \code{\link{matrix}} object.
#' @param known_symmetric a logical value indicating whether the input matrix
#' \code{x} is already known to be symmetric
#' (for this case use \code{known_symmetric = TRUE}) or not
#' (for this case use \code{known_symmetric = FALSE} (default)).
#' @param known_binary a logical value indicating whether the input matrix
#' \code{x} is already known to be a binary matrix
#' (for this case use \code{known_binary = TRUE}) or not
#' (for this case use \code{known_binary = FALSE} (default)).
#' @return The input matrix, converted to an adjacency
#' @examples
#' # look at raw matrix
#' edgynode::adjacency_matrix_test_3
#' # convert raw matrix into a edgynode adjacency matrix
#' edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
#' @seealso \code{\link{assert_adjacency}}, \code{\link{is_adjacency}}
#' @export
make_adjacency <- function(x, known_binary = FALSE){
  known_symmetric = is_symmetric(x)
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
