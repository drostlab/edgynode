infer_binarisation_threshold <- function(adj, prob = 0.95){
  # this function is a placeholder for now
  check_known_standard_error(adj)
  stats::quantile(as.vector(adj), probs = 0.95, names = FALSE)
}
