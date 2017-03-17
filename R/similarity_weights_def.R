#' Generating similarity weights for calculating_similarity
#'
#' @description This function can be used together with create_weighting_list
#'    to manually assemble similarity weights, which can be used in function
#'    calculate_similarity. Some usual weightings aur already collected in
#'    the function MDRDist_weights().
#'
#' @param numerator list of class "HLO_weighting" [which is output of
#'    create_weighting_list], that indicates, how similar classifications should
#'    be rewarded
#' @param denominator list of class "HLO_weighting" [which is output of
#'    create_weighting_list], that indicates, which classification relations
#'    should be punished
#'
#'  @details Please note the following
#'     - For reasons of scaling, it is recommented, that for each element
#'     numerator < denominator [or at least numerator < denominator * C] for
#'     some constant.
#'     - For symmetry, it should not matter, whether the first or the second
#'     observation is L or H. Therefore you should set H_L = L_H. This applies
#'     to H_O = O_H and L_O = O_L as well.
#'     - Further it does not make sense, if a thing is to something more similar
#'     than to itself. So H_same and L_same should be maximal.
#'
#' @return The function returns a list object with numerator, denuminator and
#'    class label "similarity_weights".
#'
#' @export
#'
#' @examples # see function MDRDist_weights in file similarity_weights_common.R
#'

similarity_weights <- function(numerator,
                               denominator){
  checkmate::assertClass(x = numerator,   classes = "HLO_weighting")
  checkmate::assertClass(x = denominator, classes = "HLO_weighting")

  res <- list(numerator = numerator,
              denominator = denominator)
  class(res) <- "similarity_weights"
  return(res)
}

#' Describing weights for similarity relation between different interaction categories
#'
#' @description Function that collects weights for all different interaction
#'    scemes in a HLO-table. Each HLO-cell gets a "model assumption", which
#'    mapps the letter to an integer. Here weights for any combination of theese
#'    integers are assigned.
#'
#' @param H_same weight, if both integers are equal and positive
#' @param H_H weight, if both integers are positive, but different
#' @param H_O weight, if if one integer is positive and one is 0
#' @param H_L weight, if if one integer is positive and one is negative
#' @param O_H weight, if if one integer is positive and one is 0
#' @param O_O weight, if if both integers are 0
#' @param O_L weight, if if one integer is negative and one is zero
#' @param L_H weight, if if one integer is positive and one is negative
#' @param L_O weight, if if one integer is negative and one is zero
#' @param L_L weight, if both integers are negative, but different
#' @param L_same weight, if both integers are equal and negative
#'
#' @details For reasons of symmetry, the parameters should hold
#'    H_O = O_H, H_L = L_H and L_O = O_L
#'
#' @return a list with the input parameters and class label "HLO_weighting",
#'    which can be used by function similarity_weights
#'
#' @export
#'
#' @examples # see function MDRDist_weights in file similarity_weights_common.R
#'

create_weighting_list <- function(H_same = 1,
                                  H_H = 0,
                                  H_O = 0,
                                  H_L = 0,
                                  O_H = 0,
                                  O_O = 0,
                                  O_L = 0,
                                  L_H = 0,
                                  L_O = 0,
                                  L_L = 0,
                                  L_same = 0){

  assertNumeric(x = c(H_same, H_H, H_O, H_L, O_H, O_O, O_L, L_H, L_O, L_L, L_same),
                .var.name = "some of the input parameters")
  if(H_L != L_H){
    warning(paste0("Problem when setting similarity weights.\n",
                   "Weights for 'H_L' and 'L_H' are different.\n",
                   "Therefore symmetry of the distance matrix most likely will be broken."))
  }
  if(H_O != O_H){
    warning(paste0("Problem when setting similarity weights.\n",
                   "Weights for 'H_O' and 'O_H' are different.\n",
                   "Therefore symmetry of the distance matrix most likely will be broken."))
  }
  if(O_L != L_O){
    warning(paste0("Problem when setting similarity weights.\n",
                   "Weights for 'O_L' and 'L_O' are different.\n",
                   "Therefore symmetry of the distance matrix most likely will be broken."))
  }

  res <- c(H_same = H_same,
           H_H = H_H,
           H_O = H_O,
           H_L = H_L,
           O_H = O_H,
           O_O = O_O,
           O_L = O_L,
           L_H = L_H,
           L_O = L_O,
           L_L = L_L,
           L_same = L_same)

  if(min(res) < 0){
    warning(paste0("Problem when setting similarity weights.\n",
                   "Weights for calculating similarity should be non-negative.\n",
                   sprintf("Therefore all weights are increased by %d.", min(res))))
    res <- res - min(res)
  }

  res <- as.list(res)
  class(res) <- "HLO_weighting"
  return(res)
}
