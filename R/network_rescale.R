#' @title Rescaling function customized for adjacency matrices
#' @description This function rescales to a 0 to 100 scale using the values
#' present in the matrix. It is based on the rescale function of the
#' scales package. It will make the matrix symmetric by mirroring the upper
#' half if it isn't. If negative values present, their absolute value will be
#' taken.
#' @param adj_mat an adjacency matrix to normalize.
#' @param to interval in which transformation should be done.
#' Default is \code{to = c(0, 100)}.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @export
#' @examples
#' # path to ppcor output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#' # parsing the output to an adjacency matrix
#' ppcor_parsed <- ppcor(ppcor_output)
#' # rescaling the matrix
#' rescaled <- network_rescale(ppcor_parsed)
#' # Visualize result
#' head(rescaled)

network_rescale <- function (adj_mat, to = c(0, 100)) {
  
  if (is.character(adj_mat[1,1]) == TRUE)
    warning("The first column consists of names.")
    #adj_mat <- as.matrix(adj_mat[ , 2:ncol(adj_mat)])
  
  diag(adj_mat) <- 0

  if (!isSymmetric(adj_mat))
    warning("The matrix provided as input for network_rescale() was coerced into symmetric.")
    adj_mat <- make_symmetric(as.matrix(adj_mat))

  if (dplyr::between(min(adj_mat), -1L, -0.8) && dplyr::between(max(adj_mat), 0.8, 1L))
    warning("It seems like your input matrix contains values of correlation coefficients range(-1,1).",
            " Please be aware that for negative values the absolute value will be taken before rescaling.")
    adj_mat <- abs(adj_mat)
    
  rescaled_mat <- scales::rescale(adj_mat, to = to)
  result <- as.matrix(rescaled_mat)
  
  return(result)
}
