# This file collects all methods applicable for generating artificial samples.
# Each function has the signature function(data, n){}, where
# - data is a data.frame with only integer columns and
# - n is the number of generated observations.
#
# The functions return data.frames
# - with n observations and
# - all columns of data plus one column "status" with
#       status = 0 for all observations.
# - and rownames starting with "SYNTHETIC"




#' compdist_uniform
#'
#' @description a function that generates artificial data with equally
#'    distributed variables
#'
#' @details The comparison functions are needed to apply algorithms from
#'    supervized learning. There are many possible functions approximating
#'    comparison distributions. Some are implemented here.
#'
#'    Each function has the signature function(data, n){}, where
#'      - data is a data.frame with only integer columns and
#'      - n is the number of generated observations.
#'
#'    The functions return data.frames
#'    - with n observations and
#'    - all columns of data plus one column "status" with
#'          status = 0 for all observations.
#'    - and rownames starting with "SYNTHETIC"
#'
#' @param data a data.frame with data, that should be emulated
#' @param n the number of emulated observations
#'
#' @return a data.frame like the input (but with generated observations) and an
#'    additional column status=0
#'
#' @export
#'

compdist_uniform <- function(data, n){
  res <- lapply(data, FUN = function(x, n){
    vec <- runif(n = n, min = min(x), max = max(x))
    vec <- round(vec)
    return(vec)
  },
  n = n)
  res <- as.data.frame(res)
  res$status <- 0
  rownames(res) <- paste0("SYNTHETIC",
                          sprintf("%04i",
                                  1:n))
  return(res)
}


#' compdist_estimated_uncorellated
#'
#' @description a function that generates artificial data, that are distributed
#'    like the uncorellated empirical distribution of the original data
#'
#' @details The comparison functions are needed to apply algorithms from
#'    supervized learning. There are many possible functions approximating
#'    comparison distributions. Some are implemented here.
#'
#'    Each function has the signature function(data, n){}, where
#'      - data is a data.frame with only integer columns and
#'      - n is the number of generated observations.
#'
#'    The functions return data.frames
#'    - with n observations and
#'    - all columns of data plus one column "status" with
#'          status = 0 for all observations.
#'    - and rownames starting with "SYNTHETIC"
#'
#' @param data a data.frame with data, that should be emulated
#' @param n the number of emulated observations
#'
#' @return a data.frame like the input (but with generated observations) and an
#'    additional column status=0
#'
#' @export
#'

compdist_estimated_uncorellated <- function(data, n){
  res <- lapply(data, FUN = function(x, n){
    set <- sort(unique(x))
    prob <- table(x) / sum(table(x))
    vec <- sample(x = set,
                  size = n,
                  replace = TRUE,
                  prob = prob)
    return(vec)
  },
  n = n)
  res <- as.data.frame(res)
  res$status <- 0
  rownames(res) <- paste0("SYNTHETIC",
                          sprintf("%04i",
                                  1:n))
  return(res)
}
