assert_adjacency <- function(x){
  if(!is_adjacency(x)) stop(deparse(substitute(x)), " must be an adjacency")
  invisible(NULL)
}
