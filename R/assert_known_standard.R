assert_known_standard <- function(adj){
  assert_adjacency(adj)
  if(!is_standard(adj)) stop(deparse(substitute(adj)), " must be in the standard format")
  invisible(NULL)
}
