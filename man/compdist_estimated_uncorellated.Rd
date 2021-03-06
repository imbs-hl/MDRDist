% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison_distributions.R
\name{compdist_estimated_uncorellated}
\alias{compdist_estimated_uncorellated}
\title{compdist_estimated_uncorellated}
\usage{
compdist_estimated_uncorellated(data, n)
}
\arguments{
\item{data}{a data.frame with data, that should be emulated}

\item{n}{the number of emulated observations}
}
\value{
a data.frame like the input (but with generated observations) and an
   additional column status=0
}
\description{
a function that generates artificial data, that are distributed
   like the uncorellated empirical distribution of the original data
}
\details{
The comparison functions are needed to apply algorithms from
   supervized learning. There are many possible functions approximating
   comparison distributions. Some are implemented here.

   Each function has the signature function(data, n){}, where
     - data is a data.frame with only integer columns and
     - n is the number of generated observations.

   The functions return data.frames
   - with n observations and
   - all columns of data plus one column "status" with
         status = 0 for all observations.
   - and rownames starting with "SYNTHETIC"
}

