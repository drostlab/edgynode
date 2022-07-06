#' @title Convert a numeric matrix generated with \code{\link{make_adjacency}} into a binary matrix
#' @description This function takes an adjacency matrix generated with \code{\link{make_adjacency}} as input and
#' converts it to a binary matrix using a binarization threshold.
#' @param adj an adjacency matrix converted from a raw input matrix via \code{\link{make_adjacency}}.
#' @param threshold a numeric value that is within the range
#' \code{min(adj) < threshold <= max(adj)} of the input which is then used
#' for binarisation.
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

make_binary <- function(adj, threshold, output_plot = FALSE){
  assert_adjacency(adj)

  if(output_plot){
    print(plot_adjacency_weights(adj = adj, threshold = threshold))
  }

  if(!attr(adj, "known_binary")){
    # check that the threshold is within the numeric range of the adjacency matrix
    if(threshold <= min(adj) | threshold > max(adj))
      stop("Threshold value ", threshold,
           " is not within the numeric range of your input matrix (",
           min(adj), ",", max(adj), "].")

    attr(adj, "known_binary") <- TRUE
    adj[] <- adj >= threshold
  }
  adj
}
