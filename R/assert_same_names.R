assert_same_names <- function(x1, x2){
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
