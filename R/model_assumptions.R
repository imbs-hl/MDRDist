#' Model Assumptions: all_same
#'
#' @description A model mapping, where each same letter ends up in the same category
#'
#' @details Models are a mapping from a HLO-Matrix to some integer
#'    categories, where negative values are associated with L, positive values
#'    are associated with H and 0 (zero) = O (the capital letter).
#'
#'    There are diffenent "model assumptions" (or model mappings) possible. Each
#'    uses one single input parameter (a HLO-Matrix) and returns an integer
#'    matrix of the same size. The desired model assumption is selected by
#'    function name.
#'
#' @param model A HLO-Matrix for one interaction
#'
#' @return An integer Matrix of the same size as input with categories, that
#'    replace the "H", "L" or "O"-Letters.
#'
#' @export
#'

all_same <- function(model){
  x1 <- nrow(model)
  x2 <- ncol(model)

  res <- matrix(0, nrow = x1, ncol = x2)
  rownames(res) <- rownames(model)
  colnames(res) <- colnames(model)

  res = res + as.numeric(model == "H") - as.numeric(model == "L")

  return(res)
}


#' Model Assumptions: all_different
#'
#' @description A model mapping, where each same letter ends up in a different category
#'
#' @details Models are a mapping from a HLO-Matrix to some integer
#'    categories, where negative values are associated with L, positive values
#'    are associated with H and 0 (zero) = O (the capital letter).
#'
#'    There are diffenent "model assumptions" (or model mappings) possible. Each
#'    uses one single input parameter (a HLO-Matrix) and returns an integer
#'    matrix of the same size. The desired model assumption is selected by
#'    function name.
#'
#' @param model A HLO-Matrix for one interaction
#'
#' @return An integer Matrix of the same size as input with categories, that
#'    replace the "H", "L" or "O"-Letters.
#'
#' @export
#'

all_different <- function(model){
  x1 <- nrow(model)
  x2 <- ncol(model)

  res <- all_same(model = model)
  res <- res * matrix(data = 1:(x1*x2),
                      nrow = x1,
                      ncol = x2,
                      byrow = TRUE)
  return(res)
}



#' Model Assumptions: L_same_H_different
#'
#' @description A model mapping, where each H ends up in a different category and all L in the same
#'
#' @details Models are a mapping from a HLO-Matrix to some integer
#'    categories, where negative values are associated with L, positive values
#'    are associated with H and 0 (zero) = O (the capital letter).
#'
#'    There are diffenent "model assumptions" (or model mappings) possible. Each
#'    uses one single input parameter (a HLO-Matrix) and returns an integer
#'    matrix of the same size. The desired model assumption is selected by
#'    function name.
#'
#' @param model A HLO-Matrix for one interaction
#'
#' @return An integer Matrix of the same size as input with categories, that
#'    replace the "H", "L" or "O"-Letters.
#'
#' @export
#'

L_same_H_different <- function(model){
  x1 <- nrow(model)
  x2 <- ncol(model)

  res <- all_different(model = model)
  res[res < 0] <- -1
  return(res)
}
