#' @title Force a numeric matrix generated with \code{\link{make_adjacency}} into the \pkg{edgynode} standard matrix format
#' @description This function takes an adjacency matrix as input and converts
#' it into a standard form (based on the options provided).
#' @param adj an adjacency matrix converted from a raw input matrix via \code{\link{make_adjacency}}.
#' @param max_value the maximum (positive) value that should exist in the
#' matrix; default is 1; setting this to \code{NULL} or \code{max(abs(adj))}
#' leaves the values unchanged
#' @param no_negative whether there should be negative values or just their
#' absolute value (e.g. in the case of correlations); default is \code{no_negative = TRUE}.
#' @param no_self_loops whether the values in the main diagonal of the
#' adjacency matrix should be 0; default is \code{no_negative = TRUE}.
#' @author Ilias Moutsopoulos
#' @seealso \code{\link{make_adjacency}}, \code{\link{make_symmetric}}, \code{\link{make_binary}}
#' @export

make_standard <- function(
    adj,
    max_value = 1,
    split_by_margin = NULL,
    no_negative = TRUE,
    no_self_loops = TRUE
){
  assert_adjacency(adj)
  if(no_self_loops) diag(adj) <- 0
  if(no_negative) adj <- abs(adj)
  if(!is.null(max_value)){
    if(is.null(split_by_margin)){
      adj <- adj / max(abs(adj)) * max_value
    }else{
      adj[] <- apply(X = adj, MARGIN = split_by_margin, FUN = function(x){
        x / max(abs(x)) * max_value
      })
    }
  }
  adj
}
