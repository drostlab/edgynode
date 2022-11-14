infer_binarisation_threshold <- function(adj, prob = 0.95, split_by_margin = NULL){
  # this function will be extended to other methods in the future
  assert_known_standard(adj)
  if(is.null(split_by_margin)){
    threshold <- stats::quantile(as.vector(adj), probs = prob, names = FALSE)
  }else{
    threshold <- apply(X = adj, MARGIN = split_by_margin, FUN = function(x){
      stats::quantile(x, probs = prob, names = FALSE)
    })
  }
  threshold
}
