#' @title Beeline input generation
#' @description This function writes the necessary files required to make a full
#' run of Beeline
#' @param file_path a file path to an expression count matrix.
#' @author Sergio Vasquez and Hajk-Georg Drost
#' @examples
#' # path to a valid count matrix file
#' count_matrix_address <- system.file('beeline_examples/count_matrix/1000_raw_counts.csv', package = 'edgynode')
#' # import the count matrix
#' count_matrix <- read.csv(file = count_matrix_address)
#' @export

beeline_generate <- function(file_path, 
                             time_points = 2, 
                             samples = 2, 
                             column1names = TRUE ) {
  
  if (!file.exists(file_path))
    stop("Please specify a valid file path to ", file_path, "...", .call = FALSE)
  
  original <- read.csv(file = file_path)
  names(original)[1] <- "GeneID"
  original <- cbind(rep(1, nrow(original)), original)
  names(original)[1] <- "Phylostratum"
  originalcombined <- myTAI::CollapseReplicates(original, nrep = time_points, FUN = mean, stage.names = paste0("S", 1:samples))
  write.table(originalcombined[-1], paste(file_path, "col_counts.csv"), sep = ",", quote = FALSE, col.names = TRUE, row.names = FALSE)
  
  #Reading in the count matrix and creating its pseudotime
  #Loading the data and checking which genes are actually expressed
  expr_mat <- read.csv(paste(file_path, "col_counts.csv"))
  colnames(expr_mat)[1] <- "gene_short_name"
  HSMM <- newCellDataSet(as.matrix(expr_mat[,2:ncol(expr_mat)]))
  HSMM <- detectGenes(HSMM, min_expr = 0.1)
  
  expressed_genes <- row.names(subset(fData(HSMM),
                                      num_cells_expressed >= 1))
  
  #Removing very lowly or highly expressed genes
  
  HSMM <- detectGenes(HSMM, min_expr = 0.1)
  
  #Unsupervised clustering
  HSMM <- estimateSizeFactors(HSMM)
  HSMM <- estimateDispersions(HSMM)
  disp_table <- dispersionTable(HSMM)
  unsup_clustering_genes <- subset(disp_table, mean_expression >= 0.1)
  HSMM <- setOrderingFilter(HSMM, unsup_clustering_genes$gene_id)
  plot_ordering_genes(HSMM)
  
  plot_pc_variance_explained(HSMM, return_all = F) # norm_method='log'
  HSMM <- reduceDimension(HSMM, max_components = 2, num_dim = 6,
                          reduction_method = 'DDRTree', verbose = T)
  HSMM <- clusterCells(HSMM, num_clusters = 2)
  
  #if (column1names == TRUE){
  #  values <- original[2,ncol(original)]
  #}
  
  #Extracting pseudotime values
  HSMM <- orderCells(HSMM)
  
  plot_cell_trajectory(HSMM)
  
  traj.plot <- plot_cell_trajectory(HSMM)
  point.data <- ggplot_build(traj.plot)[["plot"]][["data"]]
  
  pseudotime <- point.data %>% dplyr::select(sample_name, Pseudotime)
  
  write.csv(pseudotime, row.names = FALSE, file = paste(file_path, "pseudotime.csv"))
  
  return(original, expressed_genes)
}