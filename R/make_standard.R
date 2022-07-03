#' @title Force a matrix into a standard format
#' @description This function takes an adjacency matrix as input and converts
#' it into a standard form (based on the options provided).
#' @param adj the adjacency matrix
#' @param max_value the maximum (positive) value that should exist in the
#' matrix; default is 1; setting this to \code{NULL} or \code{max(abs(adj))}
#' leaves the values unchanged
#' @param no_negative whether there should be negative values or just their
#' absolute value (e.g. in the case of correlations); default is TRUE
#' @param no_self_loops whether the values in the main diagonal of the
#' adjacency matrix tshould be 0; default is TRUE
#' @author Ilias Moutsopoulos
#' @export

make_standard <- function(
    adj,
    max_value = 1,
    no_negative = TRUE,
    no_self_loops = TRUE
){
  check_adjacency_error(adj)
  if(no_self_loops) diag(adj) <- 0
  if(no_negative) adj <- abs(adj)
  if(!is.null(max_value)) adj <- adj / max(abs(adj)) * max_value
  adj
}
