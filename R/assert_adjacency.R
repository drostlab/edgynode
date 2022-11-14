#' @title Check whether a matrix fulfills the \pkg{edgynode} adjacency
#' matrix convention
#' @description Imports a matrix and tests whether it is an adjacency in
#' \pkg{edgynode} format.
#' @param x a \code{\link{matrix}} object.
#' @return An informative error if the test fails, NULL otherwise
#' @examples
#' # test with an already clean adjacency matrix
#' edgynode::assert_adjacency(edgynode::adjacency_clean_test_3)
#'
#' # look at raw matrix
#' edgynode::adjacency_matrix_test_3
#' # convert raw matrix into a edgynode adjacency matrix
#' clean_matrix <- edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
#' # test converted matrix
#' edgynode::assert_adjacency(clean_matrix)
#' @author Ilias Moutsopoulos and Hajk-Georg Drost
#' @seealso \code{\link{make_adjacency}}, \code{\link{is_adjacency}}
#' @export
assert_adjacency <- function(x){
  x_name <- deparse(substitute(x))
  full_error_message <- function(str){
    paste0(
      x_name,
      " is not an adjacency (",
      str,
      ").\n",
      "  You can convert an input matrix using edgynode::make_adjacency()."
      )
  }
  if(!inherits(x, "adjacency")) stop(full_error_message("not of class 'adjacency'"))
  if(!is.matrix(x)) stop(full_error_message("not a matrix"))
  if(nrow(x) != ncol(x)) stop(full_error_message("different # of rows and columns"))
  if(is.null(rownames(x))) stop(full_error_message("no row names"))
  if(is.null(colnames(x))) stop(full_error_message("no column names"))
  if(!all(rownames(x) == colnames(x))) stop(full_error_message("row/column names don't match"))
  if(!is.numeric(x)) stop(full_error_message("non-numeric values"))
  if(is.null(attr(x, "known_symmetric"))) stop(full_error_message("missing known_symmetric attribute"))
  if(is.null(attr(x, "known_binary"))) stop(full_error_message("missing known_binary attribute"))
  invisible(NULL)
}
