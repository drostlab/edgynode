compare_adj_list <- function(adj_list, metric = metric_summary_distance()){
  metric_name <- deparse(substitute(metric))
  df <- as.data.frame(t(combn(names(adj_list), 2))) |>
    poorman::rename(adj1 = V1, adj2 = V2) |>
    poorman::mutate(metric = factor(metric_name))
  df[["distance"]] <- sapply(seq_len(nrow(df)), function(i){
    adj1 <- adj_list[[df$adj1[i]]]
    adj2 <- adj_list[[df$adj2[i]]]
    metric(adj1, adj2)
  })
  df
}
