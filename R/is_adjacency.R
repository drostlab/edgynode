is_adjacency <- function(x){
  all(
    inherits(x, "adjacency"),
    is.matrix(x),
    nrow(x) == ncol(x),
    !(is.null(rownames(x)) | is.null(colnames(x))),
    all(rownames(x) == colnames(x)),
    is.numeric(x),
    !is.null(attr(x, "known_symmetric")),
    !is.null(attr(x, "known_binary"))
  )
}
