#' @title Visualize noise filter benchmarking results
#' @description This function takes a noise-filtering benchmark result generated with
#' \code{\link{network_benchmark_noise_filtering}} as input and visualizes the results as violin-plots.
#' @param network_benchmark_noise_filtering_result a benchmark result \code{tibble} generated with \code{\link{network_benchmark_noise_filtering}}.
#' @param dist_type distance method that shall be applied on the binary values for each gene. Available options are:
#' \itemize{
#' \item \code{dist_type = "hamming"}: computes the \code{\link[e1071]{hamming.distance}} for each gene between the two input matrices
#' \item \code{dist_type = "jaccard"}: computes the \code{\link[jaccard]{jaccard}} for each gene between the two input matrices
#' }
#' See \code{\link{network_dist_pairwise_genes}} for details.
#' 
#' Finally, a \code{\link{kruskal.test}} is performed to assess the statistical significance of comparison differences.
#' @param xlab x-axis label.
#' @param title a character string denoting the plot title.
#' @param y_ticks the number of ticks that shall be drawn on the y-axis.
#' @author Hajk-Georg Drost
#' @examples
#' # Benchmark GENIE3 inferred networks with raw, no_noise, and quantile_norm combinations
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
#'             dist_type = "hamming", title = "Network Inference Tool: GENIE3")
#'
#' @export

plot_network_benchmark_noise_filtering <-
  function(network_benchmark_noise_filtering_result,
           dist_type,
           xlab = "",
           title = "",
           y_ticks = 10) {
    if (!is.element(dist_type, c("hamming", "jaccard")))
      stop("Please choose a 'dist_type' that is supported by this function.",
           call. = FALSE)
    
    # compute p-value from Kruskal-Wallis rank sum test
    kruskal_p_val <- network_benchmark_noise_filtering_kruskal_test(network_benchmark_noise_filtering_result)$p.val
    
    tidy_benchmark_result <-
      transform_benchmark_into_tidy_format(network_benchmark_noise_filtering_result)
    
    Comparison <- `Hamming Distance` <- `Jaccard Similarity Coefficient` <- NULL
    
    if (dist_type == "hamming") {
      names(tidy_benchmark_result) <-
        c("GRN Tool", "Genes", "Comparison", "Hamming Distance")
      p <-
        ggplot2::ggplot(
          tidy_benchmark_result,
          ggplot2::aes(
            #x = factor(
              x = Comparison,
              # levels = c(
              #   "Original vs Filtered, Not Normalized",
              #   "Not Filtered, But Normalized vs Filtered, Normalized",
              #   "Original vs Not Filtered, But Normalized",
              #   "Original vs Filtered, Normalized"
              # )),
            y = `Hamming Distance`,
            fill = Comparison
          )
        ) +
        ggplot2::geom_violin(alpha = 0.7) + ggplot2::geom_point(size = 2.5, position = ggplot2::position_jitterdodge(),
                                                                 alpha = 0.4)  +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = xlab, y = "Hamming Distance", title = paste0(title, "Kruskal-Wallis Rank Sum Test; p = ", round(kruskal_p_val, 6))) +
        ggplot2::theme(
          title            = ggplot2::element_text(size = 16, face = "bold"),
          legend.title     = ggplot2::element_text(size = 16, face = "bold"),
          legend.text      = ggplot2::element_text(size = 8, face = "bold"),
          axis.title       = ggplot2::element_text(size = 16, face = "bold"),
          axis.text.y      = ggplot2::element_text(size = 16, face = "bold"),
          axis.text.x      = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          strip.text.x     = ggplot2::element_text(
            size           = 18,
            colour         = "black",
            face           = "bold"
          )
        ) + ggsci::scale_fill_aaas() +
        ggplot2::scale_x_discrete(breaks = NULL) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = 90,
          vjust = 1,
          hjust = 1
        )) + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = y_ticks))
    }
    
    if (dist_type == "jaccard") {
      names(tidy_benchmark_result) <-
        c("GRN Tool",
          "Genes",
          "Comparison",
          "Jaccard Similarity Coefficient")
      p <-
        ggplot2::ggplot(
          tidy_benchmark_result,
          ggplot2::aes(
          x = factor(
          Comparison, 
            levels = c(
              "Original vs Filtered, Not Normalized",
              "Not Filtered, But Normalized vs Filtered, Normalized",
              "Original vs Not Filtered, But Normalized",
              "Original vs Filtered, Normalized"
          )),
          y = `Jaccard Similarity Coefficient`, fill = Comparison)) +
        ggplot2::geom_violin(alpha = 0.7) + ggplot2::geom_point(size = 2.5, position = ggplot2::position_jitterdodge(),
                                                                 alpha = 0.4)  +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = xlab, y = "Jaccard Similarity Coefficient", title = paste0(title, " | Kruskal-Wallis Rank Sum Test; p = ", round(kruskal_p_val, 6))) +
        ggplot2::theme(
          title            = ggplot2::element_text(size = 16, face = "bold"),
          legend.title     = ggplot2::element_text(size = 16, face = "bold"),
          legend.text      = ggplot2::element_text(size = 8, face = "bold"),
          axis.title       = ggplot2::element_text(size = 16, face = "bold"),
          axis.text.y      = ggplot2::element_text(size = 16, face = "bold"),
          axis.text.x      = ggplot2::element_text(size = 8, face = "bold"),
          panel.background = ggplot2::element_blank(),
          strip.text.x     = ggplot2::element_text(
            size           = 18,
            colour         = "black",
            face           = "bold"
          )
        ) + ggsci::scale_fill_aaas() +
        ggplot2::scale_x_discrete(breaks = NULL) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(
          angle = 90,
          vjust = 1,
          hjust = 1
        )) + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = y_ticks))
    }
    
    return(p)
  }
