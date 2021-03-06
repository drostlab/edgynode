% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_network_weight_distribution_boxplot.R
\name{plot_network_weight_distribution_boxplot}
\alias{plot_network_weight_distribution_boxplot}
\title{Plot the weight distribution as boxplot}
\usage{
plot_network_weight_distribution_boxplot(
  adj_mat,
  xlab = "Edge weight",
  ylab = "Gene name",
  threshold
)
}
\arguments{
\item{adj_mat}{a weighted adjacency matrix.}

\item{xlab}{x-axis label.}

\item{ylab}{y-axis label.}

\item{threshold}{we recommended to use \code{\link{network_rescale}}
before using this function. Re-scaling will transform all values into a range [0,100].
The threshold can either be a numeric balue in the interval [0,100] or a character string
specifying the following methods for automatically determining the threshold based on the input data:
\itemize{
\item \code{threshold = "median"}: compute the \code{\link{median}} over the entire input \code{adj_mat} and use this
\code{median} value as threshold for defining all edge weights of a genes equal or below the \code{median}
value as \code{0} and all values above the \code{median} value as \code{1}.
}
The threshold value will then be drawn as vertical line in the plot.}
}
\description{
This function takes a weighted adjacency matrix and plots
the weight distribution for all edges of a gene.
}
\examples{
genie3_49_raw <- as.matrix(read.csv(
system.file("data/network_raw_49_placenta_development.csv",
 package = "edgynode"), row.names = 1))
# rescaling weighted adjacency matrix to range [0,100]
genie_rescaled <- network_rescale(genie3_49_raw)
# visualize weight distributions over all edges for each gene
# with median threshold
plot_network_weight_distribution_boxplot(genie_rescaled, threshold = "median")
}
\author{
Sergio Vasquez and Hajk-Georg Drost
}
