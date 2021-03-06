% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/network_import_ppcor.R
\name{ppcor}
\alias{ppcor}
\title{Output Parser for PPCOR}
\usage{
ppcor(file_path)
}
\arguments{
\item{file_path}{a file path to the PPCOR output file generated by BEELINE.}
}
\description{
This function parses the standard output generated by the BEELINE tool PPCOR.
}
\examples{
# path to PPCOR output file
ppcor_output <- system.file('beeline_examples/PPCOR/outFile.txt', package = 'edgynode')
# import PPCOR specific output
ppcor_parsed <- ppcor(ppcor_output)
# look at output
head(ppcor_parsed)
}
\author{
Sergio Vasquez and Hajk-Georg Drost
}
