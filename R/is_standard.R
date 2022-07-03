#' @title Check if a numeric matrix generated with \code{\link{make_adjacency}} fulfills the \pkg{edgynode} standard format
#' @description This function takes an adjacency matrix as input and checks
#' if it is in a standard form (based on the options provided).
#' @inheritParams make_standard
#' @author Ilias Moutsopoulos
#' @export

is_standard <- function(
    adj,
    max_value = 1,
    no_negative = TRUE,
    no_self_loops = TRUE
){
  assert_adjacency(adj)
  if(no_self_loops & !(sum(abs(diag(adj))) == 0)) return(FALSE)
  if(no_negative & !(all(adj >= 0))) return(FALSE)
  if(!is.null(max_value) & max(abs(adj)) != max_value) return(FALSE)
  TRUE
}
