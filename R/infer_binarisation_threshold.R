infer_binarisation_threshold <- function(adj, prob = 0.95){
  # this function will be extended to other methods in the future
  assert_known_standard(adj)
  threshold <- stats::quantile(as.vector(adj), probs = prob, names = FALSE)
  threshold
}
