#' @title Benchmarking of noise filtering and normalization impact on network inference
#' @description This function takes four types of matrices as input which
#' were generated with the same network inference tool (e.g. GENIE3), but had
#' different input data to the network inference tool in the context of
#' noise-filtering and quantile-normalization.
#' It is assumed that the four matrices inserted into this function come from
#' network inference runs with the following input data specifications:
#' \itemize{
#' \item Raw mapped RNAseq count data without noise-filtering or quantile-normalization applied (\code{adj_mat_not_filtered_not_normalized})
#' \item Raw mapped RNAseq count data with noise-filtering applied but no quantile-normalization (\code{adj_mat_filtered_and_not_normalized})
#' \item Raw mapped RNAseq count data without noise-filtering but with quantile-normalization applied (\code{adj_mat_not_filtered_but_normalized})
#' \item Raw mapped RNAseq count data with noise-filtering applied and with quantile-normalization applied (\code{adj_mat_filtered_and_normalized})
#' }
#'
#' All relevant pairwise comparisons are then performed internally with \code{\link{network_dist_pairwise_genes}} based on gene-wise
#' Hamming Distance or Jaccard Similarity Coefficient computations.
#'
#' @param adj_mat_not_filtered_not_normalized a weighted adjacency matrix derived from a network inference program
#' where no noise-filtering or quantile-normalization was applied to the input data.
#' @param adj_mat_filtered_and_not_normalized a weighted adjacency matrix derived from a network inference program
#' where noise-filtering but no quantile-normalization was applied to the input data.
#' @param adj_mat_not_filtered_but_normalized a weighted adjacency matrix derived from a network inference program
#' where no noise-filtering but quantile-normalization was applied to the input data.
#' @param adj_mat_filtered_and_normalized a weighted adjacency matrix derived from a network inference program
#' where noise-filtering and quantile-normalization were applied to the input data.
#' @param threshold we recommended to use \code{\link{network_rescale}}
#' before using this function. Re-scaling will transform all values into a range [0,100].
#' The threshold can either be a numeric value in the interval [0,100] or a character string
#' specifying the following methods for automatically determining the threshold based on the input data:
#' \itemize{
#' \item \code{threshold = "median"}: compute the \code{\link{median}} over the entire input \code{adj_mat} and use this
#' \code{median} value as threshold for defining all edge weights of a genes equal or below the \code{median}
#' value as \code{0} and all values above the \code{median} value as \code{1}.
#' }
#' See \code{\link{network_make_binary}} for details.
#' See \code{\link{network_statistics_degree_distribution_naive}} for details.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param y_ticks number of y-ticks.
#' @param print_message shall messages be printed?
#' @author lmshk and Hajk-Georg Drost
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
#'
#' # Plot Results
#' plot_network_degree_distribution_naive_comparison(
#' genie3_49_raw,
#' genie3_49_noNoiseCM_raw,
#' genie3_49_qnorm_no_noise_removed,
#' genie3_49_noNoiseCM_qnorm)

#' @export

plot_network_degree_distribution_naive_comparison <-
  function(adj_mat_not_filtered_not_normalized,
           adj_mat_filtered_and_not_normalized,
           adj_mat_not_filtered_but_normalized,
           adj_mat_filtered_and_normalized,
           threshold = "median",
           xlab = "Gene",
           ylab = "Node degree",
           y_ticks = 15,
           print_message = TRUE) {
    
    kept_genes <-
      intersect(
        rownames(adj_mat_not_filtered_not_normalized),
        rownames(adj_mat_filtered_and_normalized)
      )
    
    ### Rescaling networks and transforming continuous  edge weights into
    ### binary edge weights
    
    # Raw matrix with no noise-filter applied and no normalization applied
    adj_mat_not_filtered_not_normalized_degree <-
      network_statistics_degree_distribution_naive(
        network_make_binary(
          network_rescale(adj_mat_not_filtered_not_normalized[kept_genes, kept_genes]),
          threshold = threshold,
          print_message = print_message
        )
      )
    # Raw matrix with noise-filter applied but no normalization applied
    adj_mat_filtered_and_not_normalized_degree <-
      network_statistics_degree_distribution_naive(
        network_make_binary(
          network_rescale(adj_mat_filtered_and_not_normalized[kept_genes, kept_genes]),
          threshold = threshold,
          print_message = print_message
        )
      )
    # Raw matrix with no noise-filter applied but normalization applied
    adj_mat_not_filtered_but_normalized_degree <-
      network_statistics_degree_distribution_naive(
        network_make_binary(
          network_rescale(adj_mat_not_filtered_but_normalized[kept_genes, kept_genes]),
          threshold = threshold,
          print_message = print_message
        )
      )
    # Raw matrix with noise-filter applied and normalization applied
    adj_mat_filtered_and_normalized_degree <-
      network_statistics_degree_distribution_naive(
        network_make_binary(
          network_rescale(adj_mat_filtered_and_normalized[kept_genes, kept_genes]),
          threshold = threshold,
          print_message = print_message
        )
      )
  
    res_wide <- tibble::tibble(
      gene = kept_genes,
      `-F -N` = adj_mat_not_filtered_not_normalized_degree$node_degree,
      `+F -N` = adj_mat_filtered_and_not_normalized_degree$node_degree,
      `-F +N` = adj_mat_not_filtered_but_normalized_degree$node_degree,
      `+F +N` = adj_mat_filtered_and_normalized_degree$node_degree
    )
    
    `-F -N` <- NULL
    res_wide <- dplyr::arrange(res_wide, dplyr::desc(`-F -N`))
    
    res_long <-
      tidyr::pivot_longer(
        res_wide,
        cols = 2:ncol(res_wide),
        names_to = "Variant",
        values_to = "node_degree"
      )
    
    gene <- node_degree <- Variant <- NULL
    
    p <-
      ggplot2::ggplot(res_long,
                      ggplot2::aes(x = gene,
                                   y = node_degree,
                                   group = Variant,
                                   colour = Variant)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_line(size = 1.5, alpha = 0.4) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::theme(
        title            = ggplot2::element_text(size = 18, face = "bold"),
        legend.title     = ggplot2::element_text(size = 18, face = "bold"),
        legend.text      = ggplot2::element_text(size = 18, face = "bold"),
        axis.title       = ggplot2::element_text(size = 18, face = "bold"),
        axis.text.y      = ggplot2::element_text(size = 12, face = "bold"),
        axis.text.x      = ggplot2::element_text(size = 4, face = "bold"),
        panel.background = ggplot2::element_blank(),
        strip.text.x     = ggplot2::element_text(
          size           = 18,
          colour         = "black",
          face           = "bold"
        )
      ) + ggplot2::theme(axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 1,
        hjust = 1
      )) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = y_ticks)) +
      ggsci::scale_color_aaas()
    
    return (p)
  }
