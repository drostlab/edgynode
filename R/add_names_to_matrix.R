add_names_to_matrix <- function(x){
  if(is.null(rownames(x)) & is.null(colnames(x))){
    rownames(x) <- paste0("N", seq_len(nrow(x)))
    colnames(x) <- paste0("N", seq_len(ncol(x)))
  }else if(is.null(rownames(x))){
    rownames(x) <- colnames(x)
  }else if(is.null(colnames(x))){
    colnames(x) <- rownames(x)
  }
  x
}
