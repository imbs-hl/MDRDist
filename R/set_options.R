#' Setting parameters used in MDRDist
#'
#' @param command calling sequence for MBMDR-4.4.1 from command line. If not set,
#'    the binaries delivered with this package will be used.
#' @param p number of permutations in MBMDR's maxT-test
#' @param m number of observations that are needed for significant cell
#' @param x significance level in MBMDR-first-level test
#' @param a correction-method in MBMDR
#' @param cutting_p significance level in MBMDR-second-level test
#' @param exclude_creating_data should data used for calculating MBMDR be evaluated for distance?
#' @param fraction_of_real_data number of real observations randomly drawn,
#'    where 1 equals nrow(data)
#' @param fraction_of_artificial_data number of artificial observations randomly generated,
#'    where 1 equals nrow(data)
#' @param do_bootstrapping choosing real observations randomly with
#'    (-> bootstrapping) or without (-> subsampling) replacement
#' @param dissimilarity_exponent The exponent used when calculating dissimilarity.
#'    It is "Dissimilarity = (1 - Similarity) ** dissim_exp".
#'    Most commonly it is set to 0.5 or 1.
#'
#' @return nothing, but fills R-intern "options"-list
#' @export
#'
#' @import checkmate
#'

set_options <- function(
  command = NA,
  p = 100,
  m = 10,
  x = .1,
  a = "NONE",
  cutting_p = .05,
  fraction_of_real_data = .63,
  fraction_of_artificial_data = .63,
  do_bootstrapping = FALSE,
  exclude_creating_data = FALSE,
  dissimilarity_exponent = .5
){
  # run tests, whether input data are valid
  checkmate::assertCharacter(command)
  checkmate::assertNumber(x = p, lower = 0, upper = 10000)
  checkmate::assertNumber(x = m, lower = 0, upper = 1000)
  checkmate::assertNumber(x = x, lower = 0, upper = 1)
  checkmate::assertChoice(x = a, choices = c("NONE", "CODOMINANT", "ONESTEP", "ADDITIVE"))
  checkmate::assertNumber(x = cutting_p, lower = 0, upper = 1)
  checkmate::assertNumber(x = fraction_of_real_data, lower = 0, upper = 20)
  checkmate::assertNumber(x = fraction_of_artificial_data, lower = 0, upper = 20)
  checkmate::assertLogical(x = do_bootstrapping)
  checkmate::assertLogical(exclude_creating_data)
  checkmate::assertNumber(x = dissimilarity_exponent, lower = 0, upper = 1)

  # if not given, automatically set mbmdr-path to version delivered with package
  if(is.na(command)){
    command <- sprintf("%s/exec/mbmdr-4.4.1-%s-64bits",
                             system.file(package = "MDRDist"),
                             tolower(Sys.info()["sysname"]))
  }

  # fill options-list with parameters
  # del mdrdist
  options("mdrdist_command"                     = command)
  options("mdrdist_p"                           = p)
  options("mdrdist_m"                           = m)
  options("mdrdist_x"                           = as.character(x))
  options("mdrdist_a"                           = a)
  options("mdrdist_cutting_p"                   = cutting_p)
  options("mdrdist_fraction_of_real_data"       = fraction_of_real_data)
  options("mdrdist_fraction_of_artificial_data" = fraction_of_artificial_data)
  options("mdrdist_do_bootstrapping"            = do_bootstrapping)
  options("mdrdist_exclude_creating_data"       = exclude_creating_data)
  options("mdrdist_dissimilarity_exponent"      = dissimilarity_exponent)
}
