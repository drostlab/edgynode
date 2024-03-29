% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_delta_con.R
\name{metric_dist_delta_con}
\alias{metric_dist_delta_con}
\title{Calculate graph distance based on DeltaCon}
\usage{
metric_dist_delta_con(adj1, adj2, .MAX_POWER = 10)
}
\arguments{
\item{adj1, adj2}{the adjacency matrices}

\item{.MAX_POWER}{maximum power for matrix inversion}
}
\value{
DeltaCon score between adj1 and adj2 (a scalar)
}
\description{
Computes the distance between two graphs using the DeltaCon method.
}
\references{
D. Koutra, J. T. Vogelstein, and C. Faloutsos: DeltaCon: A Principled Massive-Graph Similarity Function. SIAM 2013: 162–170.

D. Koutra, T. Ke, U. Kang, D. H. Chau, H. K. Pao, C. Faloutsos: Unifying Guilt-by-Association Approaches: Theorems and Fast Algorithms. ECML/PKDD (2) 2011: 245-260
}
