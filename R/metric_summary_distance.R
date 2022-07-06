#' @export

metric_summary_distance <- function(
    summary_fun,
    distance_fun,
    summary_args = list(),
    distance_args = list()
){
  function(adj1, adj2){
    s1 <- do.call(summary_fun, c(list(adj1), summary_args))
    s2 <- do.call(summary_fun, c(list(adj2), summary_args))
    do.call(distance_fun, c(list(s1, s2), distance_args))
  }
}
