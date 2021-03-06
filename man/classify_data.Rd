% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify_data.R
\name{classify_data}
\alias{classify_data}
\title{Classify Data}
\usage{
classify_data(mbmdr_return, data)
}
\arguments{
\item{mbmdr_return}{the return value of mbmdr_output with additional model-mapping-column}

\item{data}{the original input data as given to mbmdr_call}
}
\value{
A data.frame of class "classified_Data", where each column represents
 one significant interaction row from mbmdr_return and the rows name the
 category of each observation in the interaction table with respect to the
 selected model assumption.
}
\description{
Classify Data
}

