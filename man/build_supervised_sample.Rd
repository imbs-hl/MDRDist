% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_supervised_sample.R
\name{build_supervised_sample}
\alias{build_supervised_sample}
\title{Build_supervised_sample}
\usage{
build_supervised_sample(data, fraction_of_real_data,
  fraction_of_artificial_data, do_bootstrapping, method)
}
\arguments{
\item{data}{A data.frame with original data}

\item{fraction_of_real_data}{number of real observations randomly drawn,
where 1 equals nrow(data)}

\item{fraction_of_artificial_data}{number of artificial observations,
where 1 equals nrow(data)}

\item{do_bootstrapping}{choosing real observations randomly with
(-> bootstrapping) or without (-> subsampling) replacement}

\item{method}{a function describing the distribution of artificial data}
}
\value{
A data.frame with a sample of original data (coded with status==1)
and artificial data (coded with status==0).
}
\description{
The function build_supervised_sample creates a dataset which combines real and
   artificial data, that could be passed to a classification algorithm.
   Some model assumptions are transmittet to function by the selection of "method",
   which is a function of class "comparison_distriobution"
   (see file comparison_distributions.R).
}

