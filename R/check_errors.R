check_adjacency_error <- function(x){
  if(!is_adjacency(x)) stop(deparse(substitute(x)), " must be an adjacency")
  invisible(NULL)
}

check_known_standard_error <- function(adj){
  check_adjacency_error(adj)
  if(!is_standard(adj)) stop(deparse(substitute(adj)), " must be in the standard format")
  invisible(NULL)
}

check_known_symmetric_error <- function(adj){
  check_known_standard_error(adj)
  if(!attr(adj, "known_symmetric")){
    stop(deparse(substitute(adj)), " must be known to be symmetric")
  }
  invisible(NULL)
}

check_known_binary_error <- function(adj){
  check_known_standard_error(adj)
  if(!attr(adj, "known_binary")){
    stop(deparse(substitute(adj)), " must be known to be binary")
  }
  invisible(NULL)
}

check_same_names_error <- function(x1, x2){
  if(is.matrix(x1) & is.matrix(x2)){
    check <- identical(rownames(x1), rownames(x2)) & identical(colnames(x1), colnames(x2))
  }else if(is.vector(x1) & is.vector(x2)){
    check <- identical(names(x1), names(x2))
  }else{
    stop(deparse(substitute(x1)), " and ", deparse(substitute(x2)),
         "must both be vectors or both be matrices")
  }
  if(!check){
    stop(deparse(substitute(x1)), " and ", deparse(substitute(x2)),
         " must have identical names")
  }
  invisible(NULL)
}
