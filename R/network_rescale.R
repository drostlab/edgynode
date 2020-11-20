#' @title Rescaling function customized for adjacency matrices
#' @description This function rescales to a 0 to 1 scale using the values
#' present in the matrix. It is based on the rescale function of the
#' scales package.
#' @param adj_mat an adjacency matrix to normalize.
#' @param to interval in which transformation should be done.
#' Default is \code{to = c(0, 100)}.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @export
#' @examples
#' # path to GENIE3 output file
#' genie3_output <- system.file('beeline_examples/GENIE3/outFile.csv', package = 'scNetworkR')
#' # parsing the output to an adjacency matrix
#' genie3_parsed <- GENIE3(genie3_output)
#' # rescaling the matrix
#' rescaled <- network_rescale(genie3_parsed)
#' # Visualize result
#' rescaled

network_rescale <- function (adj_mat, to = c(0, 100)) {
  mat_adj <- as.matrix(adj_mat[ , 2:ncol(adj_mat)])
  diag(mat_adj) <- 0

  if (!isSymmetric(mat_adj))
    warning("Please provide a symmetric matrix as 'adj_mat' input for network_rescale().", call. = FALSE)

  if (dplyr::between(min(mat_adj), -1L, -0.8) && dplyr::between(max(mat_adj), 0.8, 1L))
    warning("It seems like your input matrix contains values of correlation coefficients range(-1,1).",
            " Please be aware that scaling these values between [0,100] means that a rescaled value of 50 encodes corellation values of 0 (uncorrelated) which may make interpretations more difficult.")

  rescaled_mat <- scales::rescale(mat_adj, to = to)
  result <- dplyr::bind_cols(adj_mat[ , 1], rescaled_mat)
  return(result)
}
