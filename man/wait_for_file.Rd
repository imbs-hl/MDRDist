% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/supporting_functions.R
\name{wait_for_file}
\alias{wait_for_file}
\title{Waiting until recently written file appears}
\usage{
wait_for_file(File, max_wait = 30, timeout = 1)
}
\arguments{
\item{File}{path to the file which we are waiting for}

\item{max_wait}{timeout until assertion will be raised, if file does not appear}

\item{timeout}{timestep between two attempts to look for the file}
}
\value{
nothing but certainty, that a file is callable
}
\description{
Waiting until recently written file appears
}

