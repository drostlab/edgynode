#' @title Check whether a \code{\link{make_adjacency}} converted matrix fulfills  the \pkg{edgynode} adjacency matrix convention
#' @description Imports a matrix and tests whether it is an adjacency in \pkg{edgynode} format.
#' @param x a \code{\link{matrix}} object.
#' @examples 
#' # test with an already clean adjacency matrix
#' edgynode::is_adjacency(edgynode::adjacency_clean_test_3)
#' 
#' # look at raw matrix
#' edgynode::adjacency_matrix_test_3
#' # convert raw matrix into a edgynode adjacency matrix
#' clean_matrix <- edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
#' # test converted matrix
#' edgynode::is_adjacency(clean_matrix)
#' @author Ilias Moutsopoulos and Hajk-Georg Drost
#' @seealso \code{\link{is_symmetric}}, \code{\link{is_standard}}, \code{\link{is_binary}}
#' @export
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
