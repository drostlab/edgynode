assert_known_symmetric <- function(adj){
  assert_known_standard(adj)
  if(!attr(adj, "known_symmetric")){
    stop(deparse(substitute(adj)), " must be known to be symmetric")
  }
  invisible(NULL)
}
