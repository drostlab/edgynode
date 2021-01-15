#' @title Perform Kruskal-Wallis Rank Sum Test on noise filter benchmarking results
#' @description This function takes a noise-filtering benchmark result generated with
#' \code{\link{network_benchmark_noise_filtering}} as input and performs a Kruskal-Wallis Rank Sum Test
#' to assess the statistical signifiance between comparison differences.
#' @param network_benchmark_noise_filtering_result a benchmark result \code{tibble} generated with \code{\link{network_benchmark_noise_filtering}}
#' @author Hajk-Georg Drost
#' @examples 
# Benchmark GENIE3 inferred networks with raw, no_noise, and quantile_norm combinations
#' genie3_49_raw <- as.matrix(read.csv(
#' system.file("data/network_raw_49_placenta_development.csv",
#'  package = "edgynode"), row.names = 1))
#'
#' genie3_49_noNoiseCM_raw <- as.matrix(read.csv(
#' system.file("data/network_noNoiseCM_raw_49_placenta_development.csv",
#'  package = "edgynode"), row.names = 1))
#'
#' genie3_49_qnorm_no_noise_removed <- as.matrix(read.csv(
#' system.file("data/network_qnorm_49_placenta_development.csv",
#'  package = "edgynode"), row.names = 1))
#'
#' genie3_49_noNoiseCM_qnorm <- as.matrix(read.csv(
#' system.file("data/network_noNoiseCM_qnorm_49_placenta_development.csv",
#'  package = "edgynode"), row.names = 1))
#'
#' # Run Benchmark using Hamming distance
#' benchmark_hamming <- network_benchmark_noise_filtering(
#' genie3_49_raw,
#' genie3_49_noNoiseCM_raw,
#' genie3_49_qnorm_no_noise_removed,
#' genie3_49_noNoiseCM_qnorm,
#' dist_type = "hamming",
#' grn_tool = "GENIE3")
#'
#' # visualize at results
#' plot_network_benchmark_noise_filtering(benchmark_hamming,
#'             dist_type = "hamming", 
#'             title = "Network Inference Tool: GENIE3")
#'             
#' # perform Kruskal-Wallis Rank Sum Test
#' network_benchmark_noise_filtering_kruskal_test(benchmark_hamming)
#' @export
network_benchmark_noise_filtering_kruskal_test <- function(network_benchmark_noise_filtering_result) {
  
  res <- stats::kruskal.test(
    list(
      network_benchmark_noise_filtering_result$`-F -N / +F -N`,
      network_benchmark_noise_filtering_result$`-F -N / -F +N`,
      network_benchmark_noise_filtering_result$`-F -N / +F +N`,
      network_benchmark_noise_filtering_result$`-F +N / +F +N`,
      network_benchmark_noise_filtering_result$`-F +N / +F -N`,
      network_benchmark_noise_filtering_result$`+F +N / +F -N`
    )
  )
  
  return(res)
}