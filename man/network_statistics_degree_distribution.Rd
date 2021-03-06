% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_statistics_degree_distribution.R
\name{network_statistics_degree_distribution}
\alias{network_statistics_degree_distribution}
\title{Calculate the degree distribution from adjacency matrices}
\usage{
network_statistics_degree_distribution(
  adj_mat,
  weighted = NULL,
  mode = "undirected",
  diag = TRUE,
  add_colnames = NULL,
  add_rownames = NA,
  degree_mode = "total",
  loops = TRUE,
  normalized = FALSE,
  cumulative = TRUE,
  ...
)
}
\arguments{
\item{adj_mat}{a symmetrical adjacency matrix.}

\item{weighted}{This argument specifies whether to create a weighted graph from an adjacency matrix.
If it is NULL then an unweighted graph is created and the elements of the adjacency matrix gives the number
of edges between the vertices. If it is a character constant then for every non-zero matrix entry
an edge is created and the value of the entry is added as an edge attribute named by the weighted argument. If it is TRUE then a weighted graph is created and the name of the edge attribute will be weight. See also details below.}

\item{mode}{a character value specifying how \code{igraph} should interpret the input matrices.
Options are:
\itemize{
\item If \code{weighted = NULL}:
\itemize{
\item \code{mode = "directed"}: The graph will be directed and a matrix element gives the number of edges between two vertices.
\item \code{mode = "undirected"}: This is exactly the same as max, for convenience. Note that it is not checked whether the matrix is symmetric (default).
\item \code{mode = "upper"}: An undirected graph will be created, only the upper right triangle (including the diagonal) is used for the number of edges.
\item \code{mode = "lower"}: An undirected graph will be created, only the lower left triangle (including the diagonal) is used for creating the edges.
\item \code{mode = "max"}: An undirected graph will be created and max(A(i,j), A(j,i)) gives the number of edges.
\item \code{mode = "min"}: undirected graph will be created with min(A(i,j), A(j,i)) edges between vertex i and j.
\item \code{mode = "plus"}: undirected graph will be created with A(i,j)+A(j,i) edges between vertex i and j.
}
\item If the \code{weighted} argument is not \code{NULL} then the elements of the matrix give the weights of the edges (if they are not zero). The details depend on the value of the mode argument:
itemize{
\item \code{mode = "directed"}: The graph will be directed and a matrix element gives the edge weights.
\item \code{mode = "undirected"}: First we check that the matrix is symmetric. It is an error if not. Then only the upper triangle is used to create a weighted undirected graph (default).
\item \code{mode = "upper"}: An undirected graph will be created, only the upper right triangle (including the diagonal) is used (for the edge weights).
\item \code{mode = "lower"}: An undirected graph will be created, only the lower left triangle (including the diagonal) is used for creating the edges.
\item \code{mode = "max"}: An undirected graph will be created and max(A(i,j), A(j,i)) gives the edge weights.
\item \code{mode = "min"}: An undirected graph will be created, min(A(i,j), A(j,i)) gives the edge weights.
\item \code{mode = "plus"}: An undirected graph will be created, A(i,j)+A(j,i) gives the edge weights.
}
}}

\item{diag}{a logical value specifying whether to include the diagonal of the matrix in the calculation. If \code{diag = FALSE} then the diagonal first set to zero and then passed along the downstream functions.}

\item{add_colnames}{a character value specifying whether column names shall be added as vertex attributes. Options are:
\itemize{
\item \code{add_colnames = NULL} (default):  if present, column names are added as vertex attribute ‘name’.
\item \code{add_colnames = NA}: column names will not be added.
\item \code{add_colnames = ""}: If a character constant is specified then it gives the name of the vertex attribute to add.
}}

\item{add_rownames}{a character value specifying whether to add the row names as vertex attributes. Possible values the same as the previous argument. By default row names are not added. If ‘add.rownames’ and ‘add.colnames’ specify the same vertex attribute, then the former is ignored.}

\item{degree_mode}{Character string, “out” for out-degree, “in” for in-degree or “total” for the sum of the two. For undirected graphs this argument is ignored. “all” is a synonym of “total”.}

\item{loops}{Logical; whether the loop edges are also counted.}

\item{normalized}{Logical scalar, whether to normalize the degree. If TRUE then the result is divided by n-1, where n is the number of vertices in the graph.}

\item{cumulative}{Logical; whether the cumulative degree distribution is to be calculated.}

\item{\dots}{additional arguments passed on to \code{\link[igraph]{graph_from_adjacency_matrix}}.}
}
\description{
This function uses the \pkg{igraph} package to calculate
the degree distribution from a adjacency matrix.
}
\examples{
# path to PPCOR output file
ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
# import PPCOR output into adjacency matrix
ppcor_parsed <- ppcor(ppcor_output)
# rescaling matrix
ppcor_rescaled <- network_rescale(ppcor_parsed)
# calculate network statistics
ppcor_statistics <- network_statistics_degree_distribution(ppcor_rescaled)
# look at results
head(ppcor_statistics)
}
\author{
Sergio Vasquez and Hajk-Georg Drost
}
