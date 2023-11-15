#' @title Check if a \code{\link{make_adjacency}} converted matrix is symmetric
#' @description This function takes an adjacency matrix as input and checks if
#' it is symmetric.
#' @param adj an adjacency matrix converted from a raw input matrix via \code{\link{make_adjacency}}. 
#' @author Ilias Moutsopoulos and Hajk-Georg Drost
#' @examples 
#' # look at raw matrix
#' edgynode::adjacency_matrix_test_3
#' # convert raw matrix into a edgynode adjacency matrix
#' clean_matrix <- edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
#' # make clean matrix symmetric
#' clean_matrix_symm <- edgynode::make_symmetric(clean_matrix)
#' # look at symmetric matrix
#' clean_matrix_symm
#' # test converted matrix
#' edgynode::is_symmetric(clean_matrix_symm)
#' @seealso \code{\link{is_adjacency}}, \code{\link{is_standard}}, \code{\link{is_binary}}
#' @export

is_symmetric <- function(adj){
  if(is.null(attr(adj, "known_symmetric"))){
    attr(adj, "known_symmetric") <- isSymmetric(adj)
  } else if(attr(adj, "known_symmetric")){
    return(TRUE)
  }else{
    return(isSymmetric(adj))
  }
}
