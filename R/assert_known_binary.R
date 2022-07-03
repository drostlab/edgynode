assert_known_binary <- function(adj){
  assert_known_standard(adj)
  if(!attr(adj, "known_binary")){
    stop(deparse(substitute(adj)), " must be known to be binary")
  }
  invisible(NULL)
}
