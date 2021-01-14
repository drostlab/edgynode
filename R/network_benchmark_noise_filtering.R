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
#' @param grn_tool a character string specifying the gene regulatory network inference tool that was used to
#' generate input matrices. Default is \code{grn_tool = NA}.
#' @param threshold we recommended to use \code{\link{network_rescale}}
#' before using this function. Re-scaling will transform all values into a range [0,100].
#' The threshold can either be a numeric balue in the interval [0,100] or a character string
#' specifying the following methods for automatically determining the threshold based on the input data:
#' \itemize{
#' \item \code{threshold = "median"}: compute the \code{\link{median}} over the entire input \code{adj_mat} and use this
#' \code{median} value as threshold for defining all edge weights of a genes equal or below the \code{median}
#' value as \code{0} and all values above the \code{median} value as \code{1}.
#' }
#' See \code{\link{network_make_binary}} for details.
#' @param dist_type a distance method that shall be applied on the binary values for each gene. Available options are:
#' \itemize{
#' \item \code{dist_type = "hamming"}: computes the \code{\link[e1071]{hamming.distance}} for each gene between the two input matrices
#' \item \code{dist_type = "jaccard"}: computes the \code{\link[jaccard]{jaccard}} for each gene between the two input matrices
#' }
#' See \code{\link{network_dist_pairwise_genes}} for details.
#' @param print_message shall messages be printed?
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
#' # look at results
#' benchmark_hamming
#'
#' # Run Benchmark using Jaccard Coefficients
#' benchmark_jaccard <- network_benchmark_noise_filtering(
#' genie3_49_raw,
#' genie3_49_noNoiseCM_raw,
#' genie3_49_qnorm_no_noise_removed,
#' genie3_49_noNoiseCM_qnorm,
#' dist_type = "jaccard",
#' grn_tool = "GENIE3")
#'
#' # look at results
#' benchmark_jaccard
#' @export

network_benchmark_noise_filtering <-
  function(adj_mat_not_filtered_not_normalized,
           adj_mat_filtered_and_not_normalized,
           adj_mat_not_filtered_but_normalized,
           adj_mat_filtered_and_normalized,
           grn_tool = NA,
           threshold = "median",
           dist_type = "hamming",
           print_message = TRUE) {
    
    ### Rescaling networks and transforming continuous  edge weights into
    ### binary edge weights
    
    # Raw matrix with no noise-filter applied and no normalization applied
    adj_mat_not_filtered_not_normalized_binary <-
      network_make_binary(
        network_rescale(adj_mat_not_filtered_not_normalized),
        threshold = threshold,
        print_message = print_message
      )
    # Raw matrix with noise-filter applied but no normalization applied
    adj_mat_filtered_and_not_normalized_binary <-
      network_make_binary(
        network_rescale(adj_mat_filtered_and_not_normalized),
        threshold = threshold,
        print_message = print_message
      )
    # Raw matrix with no noise-filter applied but normalization applied
    adj_mat_not_filtered_but_normalized_binary <-
      network_make_binary(
        network_rescale(adj_mat_not_filtered_but_normalized),
        threshold = threshold,
        print_message = print_message
      )
    # Raw matrix with noise-filter applied and normalization applied
    adj_mat_filtered_and_normalized_binary <-
      network_make_binary(
        network_rescale(adj_mat_filtered_and_normalized),
        threshold = threshold,
        print_message = print_message
      )
    
    
    kept_genes <- 
      intersect(
        rownames(adj_mat_not_filtered_not_normalized_binary),
        rownames(adj_mat_filtered_and_normalized_binary)
      )
    
    
    if (print_message) {
      message("\n")
      message(
        "Comparison 1: 'adj_mat_not_filtered_not_normalized' vs 'adj_mat_filtered_and_not_normalized'"
      )
    }
    
    # Comparison 1:
    no_noise_no_qnorm_VS_noise_removed_no_qnorm <-
      network_dist_pairwise_genes(
        adj_mat_not_filtered_not_normalized_binary[kept_genes, kept_genes],
        adj_mat_filtered_and_not_normalized_binary[kept_genes, kept_genes],
        dist_type = dist_type,
        print_message = print_message
      )
    
    
    if (print_message) {
      message("\n")
      message(
        "Comparison 2: 'adj_mat_not_filtered_not_normalized' vs 'adj_mat_not_filtered_but_normalized'"
      )
    }
    
    # Comparison 2:
    no_noise_no_qnorm_VS_no_noise_with_qnorm <-
      network_dist_pairwise_genes(
        adj_mat_not_filtered_not_normalized_binary[kept_genes, kept_genes],
        adj_mat_not_filtered_but_normalized_binary[kept_genes, kept_genes],
        dist_type = dist_type,
        print_message = print_message
      )
    
    if (print_message) {
      message("\n")
      message(
        "Comparison 3: 'adj_mat_not_filtered_not_normalized' vs 'adj_mat_filtered_and_normalized'"
      )
    }
    
    # Comparison 3:
    no_noise_no_qnorm_VS_noise_removed_with_qnorm <-
      network_dist_pairwise_genes(
        adj_mat_not_filtered_not_normalized_binary[kept_genes, kept_genes],
        adj_mat_filtered_and_normalized_binary[kept_genes, kept_genes],
        dist_type = dist_type,
        print_message = print_message
      )
    
    
    if (print_message) {
      message("\n")
      message(
        "Comparison 4: 'adj_mat_not_filtered_but_normalized' vs 'adj_mat_filtered_and_normalized'"
      )
    }
    
    # Comparison 4:
    no_noise_with_qnorm_VS_noise_removed_with_qnorm <-
      network_dist_pairwise_genes(
        adj_mat_not_filtered_but_normalized_binary[kept_genes, kept_genes],
        adj_mat_filtered_and_normalized_binary[kept_genes, kept_genes],
        dist_type = dist_type,
        print_message = print_message
      )
    
    if (length(unique(c(
      length(no_noise_no_qnorm_VS_noise_removed_no_qnorm),
      length(no_noise_no_qnorm_VS_no_noise_with_qnorm),
      length(no_noise_no_qnorm_VS_noise_removed_with_qnorm),
      length(no_noise_with_qnorm_VS_noise_removed_with_qnorm)
    ))) > 1)
      stop("After pairwise inner joining some joined matrices seem to have different dimensionalities and thus different distance vector lengths that cannot be returned. Please check what might have gone wrong.")
    
    
    if (length(setdiff(names(no_noise_no_qnorm_VS_noise_removed_no_qnorm), names(no_noise_no_qnorm_VS_noise_removed_with_qnorm))) > 0)
      stop("Different gene names seem to have been removed from individual gene matrices, making the gene sets not comparable.")
    if (length(setdiff(names(no_noise_no_qnorm_VS_noise_removed_no_qnorm), names(no_noise_with_qnorm_VS_noise_removed_with_qnorm))) > 0)
      stop("Different gene names seem to have been removed from individual gene matrices, making the gene sets not comparable.")
    if (length(setdiff(names(no_noise_no_qnorm_VS_noise_removed_with_qnorm), names(no_noise_with_qnorm_VS_noise_removed_with_qnorm))) > 0)
      stop("Different gene names seem to have been removed from individual gene matrices, making the gene sets not comparable.")
    
    res <- tibble::tibble(
      grn_tool = rep(grn_tool, length(no_noise_no_qnorm_VS_noise_removed_no_qnorm)),
      genes = names(no_noise_no_qnorm_VS_noise_removed_no_qnorm),
      `Original vs Filtered, Not Normalized` = no_noise_no_qnorm_VS_noise_removed_no_qnorm,
      `Original vs Not Filtered, But Normalized` = no_noise_no_qnorm_VS_no_noise_with_qnorm,
      `Original vs Filtered, Normalized` = no_noise_no_qnorm_VS_noise_removed_with_qnorm,
      `Not Filtered, But Normalized vs Filtered, Normalized` = no_noise_with_qnorm_VS_noise_removed_with_qnorm
    )
    
    return (res) 
}
