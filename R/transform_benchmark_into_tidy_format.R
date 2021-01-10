# Helper function to transform network_benchmark_noise_filtering_result into tidy format
transform_benchmark_into_tidy_format <- function(network_benchmark_noise_filtering_result) {
  
  adj_mat_long <-
    tidyr::pivot_longer(
      tibble::as_tibble(network_benchmark_noise_filtering_result),
      cols = 3:ncol(network_benchmark_noise_filtering_result)
    )
  
  return(adj_mat_long)
}