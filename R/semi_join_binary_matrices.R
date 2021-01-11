# Internal helper function to join binary adjacency matrices
semi_join_binary_matrices <- function(x,y) {
  # For Comparison 1:  transform binary matrix to tibble to be able to perform inner join operations
  x_tibble <-
    tibble::tibble(
      genes = rownames(x),
      tibble::as_tibble(x)
    )
  
  y_tibble <-
    tibble::tibble(
      genes = rownames(y),
      tibble::as_tibble(y)
    )
  
  # determine whether or not genes are missing in either matrix
  excluded_genes_comparison <-
    setdiff(
      x_tibble$genes,
      y_tibble$genes
    )
  
  if (length(excluded_genes_comparison) > 0) {
    message("\n")
    message(
      "The following genes are present in one of the compared matrices, but not in the other."
    )
    message("GeneSet = ",
            paste0(excluded_genes_comparison, collapse = ", "))
    message("\n")
    message(
      "As a result an semi join between both matrices over the gene names will be performed."
    )
    
    # after inner join transform back to matrix for downstream processing
    # for (x,y)
    x_tibble_joined <-
      dplyr::semi_join(
        x_tibble,
        y_tibble,
        by = "genes"
      )
    
    # remove excluded genes from columns since it is a symmetric matrix
    x_tibble_joined <- dplyr::select(x_tibble_joined, -excluded_genes_comparison)
    
    x <-
      as.matrix(
        dplyr::select(
          x_tibble_joined,-genes
        )
      )
    row.names(x) <-
      x_tibble_joined$genes
    
    # for (y,x)
    y_tibble_joined <-
      dplyr::semi_join(
        y_tibble,
        x_tibble,
        by = "genes"
      )
    
    # remove excluded genes from columns since it is a symmetric matrix
    #y_tibble_joined <- dplyr::select(y_tibble_joined, -excluded_genes_comparison)
    
    y <-
      as.matrix(
        dplyr::select(
          y_tibble_joined,-genes
        )
      )
    row.names(y) <-
      y_tibble_joined$genes
  }
  
  return(list(x_mat = x, y_mat = y))
}