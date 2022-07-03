#' @title Convert a numeric matrix generated with \code{\link{make_adjacency}} into a binary matrix
#' @description This function takes an adjacency matrix generated with \code{\link{make_adjacency}} as input and
#' converts it to a binary matrix using a binarization threshold. 
#' @param adj an adjacency matrix converted from a raw input matrix via \code{\link{make_adjacency}}. 
#' @param threshold a numeric value that is within the range of the input \code{adj} matrix which is then used  for binarisation.
#' @author Ilias Moutsopoulos and Hajk-Georg Drost
#' @examples 
#' # look at raw matrix
#' edgynode::adjacency_matrix_test_3
#' # convert raw matrix into a edgynode adjacency matrix
#' edgynode_matrix <- edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
#' # convert into a binary matrix
#' edgynode_matrix_binary <- edgynode::make_binary(edgynode_matrix, threshold = 1)
#' # look at result
#' edgynode_matrix_binary
#' @export

make_binary <- function(adj, threshold){
  assert_adjacency(adj)
  
  # check that the threshold is within the numeric range of the adjacency matrix
  if (!poorman::between(threshold, min(adj), max(adj)))
    stop("Your threshold value ", threshold, " is not within the numeric range of your input matrix [", min(adj), ",", max(adj), "]." , call. = FALSE)
  
  if(!attr(adj, "known_binary")){
    attr(adj, "known_binary") <- TRUE
    adj[] <- adj >= threshold
  }
  adj
}
