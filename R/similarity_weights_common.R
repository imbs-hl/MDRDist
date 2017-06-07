#' Collection of common similarity_weights
#'
#' @description This function contains common similarity weights, that can be
#'    used as similarity_weights-parameter in function calculate_similarity.
#'
#' @param weight_name A character out of the list ("first_test", <to be extended>...)
#'    that references the desired similarity mapping.
#'
#' @return returns an elements of class similarity_weights, that has been
#'    referenced by input parameter weight_name and can be passed to
#'    "calculate_similarity".
#'
#' @export
#'
#' @examples my_weight <- MDRDist_weights("first_test")
#'

MDRDist_weights <- function(weight_name){
  checkmate::assertChoice(x = weight_name,
                          choices = c("first_test", "only_H",
                                      "S1", "S2", "S3", "S4", "S5", "S6", "S7",
                                      "L1", "L2", "L3", "L4", "L5", "L6", "L7"))
  res <- switch (weight_name,
                 "first_test" = MDRDist_weights_first_test(),
                 "S1"         = MDRDist_weights_S1(),
                 "S2"         = MDRDist_weights_S2(),
                 "S3"         = MDRDist_weights_S3(),
                 "S4"         = MDRDist_weights_S4(),
                 "S5"         = MDRDist_weights_S5(),
                 "S6"         = MDRDist_weights_S6(),
                 "S7"         = MDRDist_weights_S7(),
                 "L1"         = MDRDist_weights_L1(),
                 "L2"         = MDRDist_weights_L2(),
                 "L3"         = MDRDist_weights_L3(),
                 "L4"         = MDRDist_weights_L4(),
                 "L5"         = MDRDist_weights_L5(),
                 "L6"         = MDRDist_weights_L6(),
                 "L7"         = MDRDist_weights_L7(),
                 "only_H"     = MDRDist_weights_only_H()
  )
  return(res)
}


MDRDist_weights_first_test <- function(){
  first_test_num <- create_weighting_list(H_same = 1,
                                          H_H    = .5,
                                          L_L    = .5,
                                          L_same = 1)
  first_test_den <- create_weighting_list(H_same = 1,
                                          H_H    = .5,
                                          H_L    = .3,
                                          L_H    = .3,
                                          H_O    = 0,
                                          O_H    = 0,
                                          O_O    = 0,
                                          L_O    = 0,
                                          O_L    = 0,
                                          L_L    = .5,
                                          L_same = 1)
  first_test <- similarity_weights(numerator   = first_test_num,
                                   denominator = first_test_den)
  return(first_test)
}

MDRDist_weights_S1 <- function(){
  S_num <- create_weighting_list(H_same = 1,
                                 H_H    = 0,
                                 H_L    = 0,
                                 L_H    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S_den <- create_weighting_list(H_same = 1,
                                 H_H    = 1,
                                 H_L    = 1,
                                 L_H    = 1,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 1,
                                 L_same = 1)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}

MDRDist_weights_S2 <- function(){
  S_num <- create_weighting_list(H_same = 1,
                                 H_H    = 0,
                                 H_L    = 0,
                                 L_H    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 1)
  S_den <- create_weighting_list(H_same = 1,
                                 H_H    = 1,
                                 H_L    = 1,
                                 L_H    = 1,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 1,
                                 L_same = 1)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}

MDRDist_weights_S3 <- function(){
  S_num <- create_weighting_list(H_same = 1,
                                 H_H    = 0,
                                 H_L    = 0,
                                 L_H    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 1)
  S_den <- create_weighting_list(H_same = 1,
                                 H_H    = 1,
                                 H_L    = 2,
                                 L_H    = 2,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 1,
                                 L_same = 1)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}

MDRDist_weights_S4 <- function(){
  S_num <- create_weighting_list(H_same = 2,
                                 H_H    = 1,
                                 H_L    = 0,
                                 L_H    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S_den <- create_weighting_list(H_same = 2,
                                 H_H    = 1,
                                 H_L    = 1,
                                 L_H    = 1,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 1,
                                 L_same = 1)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}

MDRDist_weights_S5 <- function(){
  S_num <- create_weighting_list(H_same = 1,
                                 H_H    = 0,
                                 H_L    = 0,
                                 L_H    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S_den <- create_weighting_list(H_same = 1,
                                 H_H    = 1,
                                 H_L    = 1,
                                 L_H    = 1,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}


MDRDist_weights_S6 <- function(){
  S_num <- create_weighting_list(H_same = 1,
                                 H_H    = 0,
                                 H_L    = 0,
                                 L_H    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S_den <- create_weighting_list(H_same = 1,
                                 H_H    = 1,
                                 H_L    = .5,
                                 L_H    = .5,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}


MDRDist_weights_S7 <- function(){
  S_num <- create_weighting_list(H_same = 1,
                                 H_H    = 0,
                                 H_L    = 0,
                                 L_H    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S_den <- create_weighting_list(H_same = 1,
                                 H_H    = 1,
                                 H_L    = 2,
                                 L_H    = 2,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}


MDRDist_weights_only_H <- function(){
  S_num <- create_weighting_list(H_same = 1,
                                 H_H    = 0,
                                 H_L    = 0,
                                 L_H    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S_den <- create_weighting_list(H_same = 1,
                                 H_H    = 1,
                                 H_L    = 0,
                                 L_H    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 O_O    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 L_L    = 0,
                                 L_same = 0)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}


MDRDist_weights_L1 <- function(){
  S_num <- create_weighting_list(L_same = 1,
                                 L_L    = 0,
                                 L_H    = 0,
                                 H_L    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 0)
  S_den <- create_weighting_list(L_same = 1,
                                 L_L    = 1,
                                 L_H    = 1,
                                 H_L    = 1,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 1,
                                 H_same = 1)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}

MDRDist_weights_L2 <- function(){
  S_num <- create_weighting_list(L_same = 1,
                                 L_L    = 0,
                                 L_H    = 0,
                                 H_L    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 1)
  S_den <- create_weighting_list(L_same = 1,
                                 L_L    = 1,
                                 L_H    = 1,
                                 H_L    = 1,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 1,
                                 H_same = 1)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}

MDRDist_weights_L3 <- function(){
  S_num <- create_weighting_list(L_same = 1,
                                 L_L    = 0,
                                 L_H    = 0,
                                 H_L    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 1)
  S_den <- create_weighting_list(L_same = 1,
                                 L_L    = 1,
                                 L_H    = 2,
                                 H_L    = 2,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 1,
                                 H_same = 1)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}

MDRDist_weights_L4 <- function(){
  S_num <- create_weighting_list(L_same = 2,
                                 L_L    = 1,
                                 L_H    = 0,
                                 H_L    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 0)
  S_den <- create_weighting_list(L_same = 2,
                                 L_L    = 1,
                                 L_H    = 1,
                                 H_L    = 1,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 1,
                                 H_same = 1)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}

MDRDist_weights_L5 <- function(){
  S_num <- create_weighting_list(L_same = 1,
                                 L_L    = 0,
                                 L_H    = 0,
                                 H_L    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 0)
  S_den <- create_weighting_list(L_same = 1,
                                 L_L    = 1,
                                 L_H    = 1,
                                 H_L    = 1,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 0)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}


MDRDist_weights_L6 <- function(){
  S_num <- create_weighting_list(L_same = 1,
                                 L_L    = 0,
                                 L_H    = 0,
                                 H_L    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 0)
  S_den <- create_weighting_list(L_same = 1,
                                 L_L    = 1,
                                 L_H    = .5,
                                 H_L    = .5,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 0)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}


MDRDist_weights_L7 <- function(){
  S_num <- create_weighting_list(L_same = 1,
                                 L_L    = 0,
                                 L_H    = 0,
                                 H_L    = 0,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 0)
  S_den <- create_weighting_list(L_same = 1,
                                 L_L    = 1,
                                 L_H    = 2,
                                 H_L    = 2,
                                 L_O    = 0,
                                 O_L    = 0,
                                 O_O    = 0,
                                 H_O    = 0,
                                 O_H    = 0,
                                 H_H    = 0,
                                 H_same = 0)
  S <- similarity_weights(numerator   = S_num,
                          denominator = S_den)
  return(S)
}