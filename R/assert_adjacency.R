assert_adjacency <- function(x){
  if(!is_adjacency(x)) stop(deparse(substitute(x)), " must be an adjacency matrix with numeric values. You can convert your input matrix using edgynode::make_adjacency().")
  invisible(NULL)
}
