#' @title Calculate the naive degree distribution from binary adjacency matrices
#' @description This function calculates
#' the degree distribution from a binary adjacency matrix.
#' @param adj_mat a symmetrical binary adjacency matrix.
#' @param store_top_frac shall a fraction of the top \code{N} genes with highest naive node degree be
#' stored in a output file? Default is  \code{store_top_frac = FALSE}.
#' @param top_frac a fraction (between [0,1]) of top genes that shall be stored in the output file (if \code{store_top_frac = TRUE}).
#' @param store_path a file  path where the output file shall be stored. Default is \code{store_path = file.path(tempdir(), "example_network_statistics_degree_distribution_naive.tsv")}.
#' @author Hajk-Georg Drost
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
#' 
#' # store top genes with highest node degree at tempdir()
#'  network_statistics_degree_distribution_naive(ppcor_binary_adj_mat, 
#'    store_top_frac = TRUE, top_frac = 0.1)
#' @export

network_statistics_degree_distribution_naive <-
  function (adj_mat,
            store_top_frac = FALSE,
            top_frac = 0.1,
            store_path = file.path(tempdir(),
                                   "example_network_statistics_degree_distribution_naive.tsv")) {
    if (!isSymmetric(adj_mat))
      stop(
        "Please provide a symmetric matrix as 'adj_mat' input for network_statistics().",
        call. = FALSE
      )
    
    if (!is_binary_matrix(adj_mat))
      stop(
        "Please make sure to insert a binary adjacency matrix, e.g. using edgynode::network_make_binary().",
        call. = FALSE
      )
    
    #distributions and cohesive blocks need graph objects
    res_row <- sort(rowSums(adj_mat))
    res_col <- sort(colSums(adj_mat))
    
    if (!all(res_row == res_col))
      stop(
        "It seems like rowSums and colSums are not equal. Please check what could have gone wrong.",
        call. = FALSE
      )
    res <-
      tibble::tibble(gene_name = names(res_row),
                     node_degree = as.integer(res_row))
    
    if (store_top_frac) {
      readr::write_tsv(dplyr::top_frac(res, top_frac),
                       file = store_path,
                       col_names = TRUE)
      message("The top ", top_frac, "% of genes with highest node degree were stored at: ", store_path)
    }
    
    return(res)
  }
