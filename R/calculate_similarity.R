#' Calculating similarity from HLO-classification
#'
#' @description This function mapps the HLO-classifications to a similarity matrix
#'
#' @param classified_data A data.frame as returned by function classify_data
#'    with observations in the rows, interactions in the columns and filled
#'    with integers, describing the interaction with respect to the choosen
#'    model assumption.
#' @param similarity_weights A set of numbers, how to evaluate the similarity
#'    between different interaction categories ... of class similarity_weights,
#'    e.g. comming from the functions similarity_weights() or MDRDist_weights().
#'
#' @return a similartiy matrix in the form
#'    [pairwise sum of numerator similarity weights] / [pairwise sum of denominator similarity weights]
#'    for all pairs of observations over all significant interactions
#'
#' @export
#'

calculate_similarity <- function(classified_data, similarity_weights){
### test things
  checkmate::assertDataFrame(x = classified_data, types = "numeric")
  checkmate::assertClass(x = similarity_weights, classes = "similarity_weights")

  numerators   <- parallelMap::parallelLapply(xs = classified_data,
                       fun = evaluate_one_run,
                       weight = similarity_weights$numerator)
  denominators <- parallelMap::parallelLapply(xs = classified_data,
                         fun = evaluate_one_run,
                         weight = similarity_weights$denominator)

  numerators_sum <- Reduce("+", numerators)
  denominators_sum <- pmax(Reduce("+", denominators), exp(-100))

  res <- numerators_sum / denominators_sum

  scale_factor <- ifelse(test = diag(res) != 0,
                         yes  = 1 / sqrt(diag(res)),
                         no   = 0)
  scale_matrix <- diag(scale_factor)
  res <- scale_matrix %*% res %*% scale_matrix
  res <- pmin(res, 1)

  rownames(res) <- rownames(classified_data)
  colnames(res) <- rownames(classified_data)
  return(res)
}


evaluate_one_run <- function(obs, weight){
  observed_expressions <- unique(obs)
  M <- matrix(data = 0,
              nrow = length(obs),
              ncol = length(obs),
              dimnames = list(names(obs),
                              names(obs)))

  for (i in observed_expressions[observed_expressions>0]){
    A <- (obs == i)
    B <- (obs > 0) & (obs != i)

    A[is.na(A)] <- 0
    B[is.na(B)] <- 0

    M <- M +
      weight$H_same * (A %*% t(A)) +
      weight$H_H    * (A %*% t(B))
  }

  for (i in observed_expressions[observed_expressions<0]){
    A <- (obs == i)
    B <- (obs < 0) & (obs != i)

    A[is.na(A)] <- 0
    B[is.na(B)] <- 0

    M <- M +
      weight$L_same * (A %*% t(A)) +
      weight$L_L    * (A %*% t(B))
  }

  H <- (obs > 0)
  O <- (obs == 0)
  L <- (obs < 0)

  H[is.na(H)] <- 0
  L[is.na(L)] <- 0
  O[is.na(O)] <- 0

  M <- M +
    weight$O_O    * (O %*% t(O)) +
    weight$L_O    * (L %*% t(O)) +
    weight$O_L    * (O %*% t(L)) +
    weight$L_H    * (L %*% t(H)) +
    weight$H_L    * (H %*% t(L)) +
    weight$H_O    * (H %*% t(O)) +
    weight$O_H    * (O %*% t(H))

 return(M)
}