#' @title Calculate the naive degree distribution from binary adjacency matrices 
#' @description This function calculates
#' the degree distribution from a binary adjacency matrix.
#' @param adj_mat a symmetrical binary adjacency matrix.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples
#' # path to PPCOR output file
#' ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
#' # import PPCOR output into adjacency matrix
#' ppcor_parsed <- ppcor(ppcor_output)
#' # rescaling matrix
#' ppcor_rescaled <- network_rescale(ppcor_parsed)
#' # make weighted adjacency matrix binary
#' ppcor_binary_adj_mat <- network_make_binary(ppcor_rescaled, threshold = 70)
#' # calculate network statistics
#' ppcor_statistics <- network_statistics_degree_distribution_naive(ppcor_binary_adj_mat)
#' # look at results
#' ppcor_statistics
#' @export

network_statistics_degree_distribution_naive <- function (adj_mat) {
    
    if (!isSymmetric(adj_mat))
      stop(
        "Please provide a symmetric matrix as 'adj_mat' input for network_statistics().",
        call. = FALSE
      )
  
    if (!is_binary_matrix(adj_mat))
      stop("Please make sure to insert a binary adjacency matrix, e.g. using edgynode::network_make_binary().", call. = FALSE)
    
    #distributions and cohesive blocks need graph objects
    res_row <- sort(rowSums(adj_mat))
    res_col <- sort(colSums(adj_mat))
    
    if(!all(res_row == res_col))
      stop("It seems like rowSums and colSums are not equal. Please check what could have gone wrong.", call. = FALSE)
    res <- tibble::tibble(gene_name = names(res_row), node_degree = as.integer(res_row))
    return(res)
}



