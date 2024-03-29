% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_binary.R
\name{make_binary}
\alias{make_binary}
\title{Convert a numeric matrix generated with \code{\link{make_adjacency}} into a binary matrix}
\usage{
make_binary(adj, threshold, output_plot = FALSE)
}
\arguments{
\item{adj}{an adjacency matrix converted from a raw input matrix via \code{\link{make_adjacency}}.}

\item{threshold}{a numeric value that is within the range
\code{min(adj) < threshold <= max(adj)} of the input which is then used
for binarisation.}
}
\description{
This function takes an adjacency matrix generated with \code{\link{make_adjacency}} as input and
converts it to a binary matrix using a binarization threshold.
}
\examples{
# look at raw matrix
edgynode::adjacency_matrix_test_3
# convert raw matrix into a edgynode adjacency matrix
edgynode_matrix <- edgynode::make_adjacency(edgynode::adjacency_matrix_test_3)
# convert into a binary matrix
edgynode_matrix_binary <- edgynode::make_binary(edgynode_matrix, threshold = 1)
# look at result
edgynode_matrix_binary
}
\author{
Ilias Moutsopoulos and Hajk-Georg Drost
}
