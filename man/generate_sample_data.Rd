% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_sample_data.R
\name{generate_sample_data}
\alias{generate_sample_data}
\title{Generate genetic sample data}
\usage{
generate_sample_data()
}
\value{
The function returns a list
\item{\code{$data}}{A \code{data.frame} with 20 observed markers and about 630 individuals. The Markers are called
\itemize{
\item{\code{snpA, snpB, snpC, snpD} with MAF 0.2}
\item{\code{noise1 to noise4} with MAF 0.1}
\item{\code{noise5 to noise12} with MAF 0.2}
\item{\code{noise13 to noise16} with MAF 0.3} }}
\item{\code{$classification}}{a vector containing for each observation, in which cluster it has been generated}
}
\description{
This function generates a data set that could be used for testing the algorithms.
}
\details{
This function generates a set of unclassified data and a vector to evaluate the clustering.
The data should be interpreted as two hidden clusters. For each cluster \code{n=2000} individuals are generated,
of which some become deseased. The disease appears, if
\itemize{
\item{for the first cluster with chance 80\%, if snpA > 1 and snpB > 1, with chance 5\% otherwise and}
\item{for the second cluster with chance 80\%, if snpC > 1 and snpD > 1, with chance 5\% otherwise.}
}
}

