collect_summary_vectors_to_df <- function(x){
  vec_to_df <- function(vec, id = "stat"){
    if(is.null(names(vec))) stop("input vector(s) must be named")
    df <- data.frame(
      id = id,
      name = factor(names(vec), levels = names(vec)),
      xnum = seq_along(vec),
      value = vec
    )
  }
  if(!is.list(x)){
    df <- vec_to_df(x)
  }else{
    if(is.null(names(x))) names(x) <- seq_along(x)
    df <- data.frame()
    vec_names <- names(x[[1]])
    for(i in seq_along(x)){
      vec <- x[[i]]
      if(!identical(names(vec), vec_names)){
        stop("all input vectors must have the same names")
      }
      df <- rbind(df, vec_to_df(vec, names(x)[i]))
    }
  }
  df
}
