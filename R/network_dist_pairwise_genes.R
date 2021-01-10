#' @title Comparison of binary adjacency weights of the same gene in two different networks
#' @description Compute the pairwise \code{Hamming} or \code{Jaccard} distance between binary edge weights of the 
#' same genes in two different network topologies (denoted as \code{adj_mat_qry} and \code{adj_mat_sbj}).
#' @param adj_mat_qry a binary adjacency matrix generated with \code{\link{network_rescale}} and \code{\link{network_make_binary}}.
#' @param adj_mat_sbj a binary adjacency matrix with the same genes as in \code{adj_mat_qry}, but with differnt binary edge weights generated with \code{\link{network_rescale}} and \code{\link{network_make_binary}}.
#' @param print_message shall massages be printed? Default is \code{print_message = TRUE}.
#' @param dist_method a distance method that shall be applied on the binary values for each gene. Available options are:
#' \itemize{
#' \item \code{dist_type = "hamming"}: computes the \code{\link[e1071]{hamming.distance}} for each gene between the two input matrices
#' \item \code{dist_type = "jaccard"}: computes the \code{\link[jaccard]{jaccard}} for each gene between the two input matrices
#' }
#' @author Hajk-Georg Drost
#' @examples 
#' ## Import and rescale PIDC network
#' # path to PIDC output file
#' pidc_output <- system.file('beeline_examples/PIDC/outFile.txt', package = 'edgynode')
#' # import PIDC specific output
#' pidc_parsed <- pidc(pidc_output)
#' #Set diagonal values
#' diag(pidc_parsed) <- 1
#' # rescaling PIDC output
#' pidc_rescaled <- network_rescale(pidc_parsed)
#' pidc_binary_adj_mat <- network_make_binary(pidc_rescaled, threshold = "median")
#' # compute hamming distances for each gene between input matrices
#' network_dist_pairwise_genes(pidc_binary_adj_mat, 
#' pidc_binary_adj_mat, dist_type = "hamming")
#' 
#' # compute jaccard distances for each gene between input matrices
#' network_dist_pairwise_genes(pidc_binary_adj_mat, 
#' pidc_binary_adj_mat, dist_type = "jaccard")
#' @export
network_dist_pairwise_genes <- function(adj_mat_qry, adj_mat_sbj, dist_type = "hamming", print_message = TRUE) {
  
  if (!all(dim(adj_mat_qry) == dim(adj_mat_sbj)))
    stop("Please make sure that your query and subject matrices have the same dimensionality.", call. = FALSE)
  
  if (!is.element(dist_type, c("hamming", "jaccard")))
    stop("Please select a dist_method that is supported by this function.", call. = FALSE)
  
  if (print_message) {
    message("- adj_mat_qry: nrow = (", nrow(adj_mat_qry), ") and ncol(", ncol(adj_mat_qry), ")")
    message("- adj_mat_sbj: nrow = (", nrow(adj_mat_sbj), ") and ncol(", ncol(adj_mat_sbj), ")")
  }
    
  if (dist_type == "hamming") {
    hamming_dist_results <- vector("numeric", nrow(adj_mat_qry))
    
    if (print_message)
      message("The Hamming Distances for each gene between the two input matrices are computed.")
    
    for (i in seq_len(nrow(adj_mat_qry))){
      hamming_dist_results[i] <- e1071::hamming.distance(adj_mat_qry[i, ], adj_mat_sbj[i, ])
    }
    names(hamming_dist_results) <- paste0(rownames(adj_mat_qry), "_qry_vs_", rownames(adj_mat_sbj), "_sbj")
    return(hamming_dist_results)
  }
  
  if (dist_type == "jaccard") {
    jaccard_dist_results <- vector("numeric", nrow(adj_mat_qry))
    
    if (print_message)
      message("The Jaccard Similarity Coefficients for each gene between the two input matrices are computed.")
    
    for (i in seq_len(nrow(adj_mat_qry))){
      jaccard_dist_results[i] <- jaccard(adj_mat_qry[i, ], adj_mat_sbj[i, ])
    }
    names(jaccard_dist_results) <- paste0(rownames(adj_mat_qry), "_qry_vs_", rownames(adj_mat_sbj), "_sbj")
    return(jaccard_dist_results)
  }
}