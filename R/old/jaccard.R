#' @title Compute a Jaccard/Tanimoto similarity coefficient between two vectors
#' @param x a binary vector
#' @param y a binary vector
#' @param center whether to center the Jaccard/Tanimoto coefficient by its expectation
#' @param px probability of successes in \code{x} (optional)
#' @param py probability of successes in \code{y} (optional)
#' @author Hajk-Georg Drost
#' @references The initial implementation was taken from the \pkg{jaccard} package
#' by Neo Christopher Chung, Błażej Miasojedow, Michał Startek, Anna Gambin which is 
#' currently not availabe for R version >4.0 due to a unmaintained package dependency \pkg{qvalue}
#'and thus the source code of the function \code{jaccard} was modified for use in this package.
#' @export
#' @examples
#' set.seed(537)
#' x = rbinom(50,1,.5)
#' y = rbinom(50,1,.4)
#' jaccard(x,y)
jaccard <- function(x,
                    y,
                    center = FALSE,
                    px = NULL,
                    py = NULL) {
  if (length(x) != length(y)) {
    stop("The input vectors x and y do not have the same length.", call. = FALSE)
  }
  
  if (is.null(px) | is.null(py)) {
    px <- mean(x)
    py <- mean(y)
  }
  
  sumxy <- sum(x & y)
  unionxy <- sum(x) + sum(y) - sumxy
  if (unionxy == 0) {
    j <- (px * py) / (px + py - px * py)
  } else {
    j <- sumxy / unionxy
  }
  if (center == FALSE) {
    return(j)
  } else {
    return(j - (px * py) / (px + py - px * py))
  }
}