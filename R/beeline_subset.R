#Function to subset counts into timepoints
beeline_subset <- function(counts, subsets, samples, name){
  colnames(counts)[1] <- ""
  colvector <- 1:subsets*samples+1
  
  filenames <- as.list(1:subsets)
  for(i in length(subsets)){
    paste("counts_subset", name, "_", i, sep = "") -> filenames[i]
    var <- counts[,c(samples*i, 1+samples*i)]
  }
  write.csv(counts_subset_raw1000_0, row.names = FALSE, file = "counts_subset_raw1000_0.csv")
  write.csv(counts_subset_raw1000_1, row.names = FALSE, file = "counts_subset_raw1000_1.csv")
  write.csv(counts_subset_raw1000_2, row.names = FALSE, file = "counts_subset_raw1000_2.csv")
  write.csv(counts_subset_raw1000_3, row.names = FALSE, file = "counts_subset_raw1000_3.csv")
  write.csv(counts_subset_raw1000_4, row.names = FALSE, file = "counts_subset_raw1000_4.csv")
  write.csv(counts_subset_raw1000_5, row.names = FALSE, file = "counts_subset_raw1000_5.csv")
  write.csv(counts_subset_raw1000_6, row.names = FALSE, file = "counts_subset_raw1000_6.csv")
  write.csv(counts_subset_raw1000_7, row.names = FALSE, file = "counts_subset_raw1000_7.csv")
}