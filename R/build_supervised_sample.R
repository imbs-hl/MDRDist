#' Build_supervised_sample
#'
#' @description The function build_supervised_sample creates a dataset which combines real and
#'    artificial data, that could be passed to a classification algorithm.
#'    Some model assumptions are transmittet to function by the selection of "method",
#'    which is a function of class "comparison_distriobution"
#'    (see file comparison_distributions.R).
#'
#' @param data A data.frame with original data
#' @param fraction_of_real_data number of real observations randomly drawn,
#'    where 1 equals nrow(data)
#' @param fraction_of_artificial_data number of artificial observations,
#'    where 1 equals nrow(data)
#' @param do_bootstrapping choosing real observations randomly with
#'    (-> bootstrapping) or without (-> subsampling) replacement
#' @param method a function describing the distribution of artificial data
#'
#' @return A data.frame with a sample of original data (coded with status==1)
#' and artificial data (coded with status==0).
#'
#' @export
#'

build_supervised_sample <- function(data,
                                    fraction_of_real_data,
                                    fraction_of_artificial_data,
                                    do_bootstrapping,
                                    method){
  # test input parameters are valid
  # test method is comparison_distribution_function
  # test parameters describe at least one data point
  return(rbind( select_random_subsample( data = data,
                                         s = fraction_of_real_data * nrow(data),
                                         bootstrap = do_bootstrapping),
                generate_artificial_data(data = data,
                                         n = fraction_of_artificial_data * nrow(data),
                                         method = method)))
}

select_random_subsample <- function(data, s, bootstrap){
  subsample <- sample(x = 1:nrow(data),
                      size = s,
                      replace = bootstrap)
  subsample <- sort(subsample)
  res <- data[subsample, ]
  res$status <- 1
  return(res)
}

generate_artificial_data <- function(data, n, method){
  return(method(data = data, n = n))
}