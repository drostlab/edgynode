infer_binarisation_threshold <- function(adj, prob = 0.95){
  # this function is a placeholder for now
  assert_known_standard(adj)
  stats::quantile(as.vector(adj), probs = prob, names = FALSE)
}
