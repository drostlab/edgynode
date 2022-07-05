#' @title Check whether a matrix fulfills  the \pkg{edgynode}
#' adjacency matrix convention
#' @description Tests whether a matrix is an adjacency in \pkg{edgynode} format.
#' @param x a \code{\link{matrix}} object.
#' @return The result of the test (TRUE/FALSE)
#' @examples
#' # test with an already clean adjacency matrix
#' edgynode::is_adjacency(edgynode::adjacency_clean_test_3)
#' # look at raw matrix
#' edgynode::adjacency_matrix_test_3
#' # convert raw matrix into a edgynode adjacency matrix
#' clean_matrix <- edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
#' # test converted matrix
#' edgynode::is_adjacency(clean_matrix)
#' @author Ilias Moutsopoulos and Hajk-Georg Drost
#' @seealso \code{\link{make_adjacency}}, \code{\link{assert_adjacency}}
#' @export
is_adjacency <- function(x){
  !inherits(tryCatch(assert_adjacency(x), error = function(e) e), "error")
}
