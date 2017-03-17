#' Reading MBMDR-files
#'
#' @description
#' The function \code{call_mbmdr} has created some files (including the calculated MBMDR-models) and stored
#' them on hard drive permanently.
#'
#' The aim of this function is to reload all data properly, even after
#' memory has been cleared (e.g. by shutdown of your machine).
#'
#' @details
#' In the directory given by parameter \code{where_to_find}, the following files are expected
#' \itemize{
#' \item{\code{input.mbmdr} was the source file for MBMDR-4.4.1}
#' \item{\code{output.table} is a summary of the calculations from MBMDR-4.4.1}
#' \item{\code{output.models} is a file that encodes the MBMDR-models as linear form}
#' \item{\code{generating_sample} is a list containing the rownames of the observations used for calculating mbmdr-model}
#' \item{\code{included.categories} is a set of lists, that contain variable names and the categories used by this variable}
#' }
#' Do not edit theese files by hand.
#'
#' @param where_to_find The directory in which the MBMDR-files are stored, encoded as \code{character}.
#' The directories must contain the files named in details below. If \code{where_to_find} is a vector,
#' each element will be evaluated and the results will be merged.
#'
#' @return A data.frame of class "mbmdr_output", with the columns
#' \item{\code{$First_Marker}}{ The name of first variable in interaction}
#' \item{\code{$Second_Marker}}{ The name of second variable in interaction}
#' \item{\code{$Chi.square}}{ The strength of interaction measured by chi square test}
#' \item{\code{$p.value}}{ The strength of interaction measured by significance level in maxT-test}
#' \item{\code{$source}}{ The directory, in which MBMDR stores the files}
#' \item{\code{$models}}{ A matrix showing the mbmdr-tables.}
#'
#' @export
#'

read_mbmdr <- function(where_to_find){
  #this function reads mbmdr output and transforms it into a data.frame

  if(testVector(x = where_to_find,
                min.len = 2)){
    return(do.call(what = rbind.data.frame,
                   args = lapply( X  = where_to_find,
                                  FUN = read_mbmdr)))
  }

  checkmate::assertDirectoryExists(x = where_to_find,
                                   access = "r")

  file_exist1 <- checkmate::testFileExists(x = file.path(where_to_find,
                                                         "CHECK"))
  file_exist2 <- checkmate::testFileExists(x = file.path(where_to_find,
                                                         "output.table"))
  file_exist3 <- checkmate::testFileExists(x = file.path(where_to_find,
                                                         "output.models"))
  file_exist4 <- TRUE

  if(file_exist1){
    model_check <- readLines(file.path(where_to_find,
                                       "CHECK"))
    file_exist4 <- (model_check == "MB-MDR completed successfully.")
  }

  files_ok <- all(file_exist1,
                  file_exist2,
                  file_exist3,
                  file_exist4)

  if(files_ok){
    res <-read.table(file.path(where_to_find,
                               "output.table"),
                     header = FALSE,
                     skip = 3,
                     col.names = c("First_Marker",
                                   "Second_Marker",
                                   "Chi.square",
                                   "p.value"))
    res$source <- where_to_find

    model_source <- readLines(file.path(where_to_find,
                                        "output.models"))
    models <- gsub(pattern = "['\",()]", replacement = "", x = model_source)
    # fix for bug in MBMDR:
    # if -a = "NONE", H and L are switched
    if(options("mbmdr_a") == "NONE"){
      models <- gsub(pattern = "H", replacement = "Y", x = models, fixed = TRUE)
      models <- gsub(pattern = "L", replacement = "H", x = models, fixed = TRUE)
      models <- gsub(pattern = "Y", replacement = "L", x = models, fixed = TRUE)
    }
    # end bugfix
    models <- lapply(X = models,
                     FUN = function(x){
                       res <- strsplit(x = x, split = " ")
                       res <- unlist(res)
                       return(res)
                     })
    # Test whether models can be matched to table by Variable names
    Variables <- (sapply(models, function(x){x[1:2]}))
    first_marker_recognized <- all(Variables[1, ] == res$"First_Marker")
    second_marker_recognized <- all(Variables[2, ] == res$"Second_Marker")
    if (!(first_marker_recognized & second_marker_recognized)){
      stop(paste("Files in", where_to_find, "corrupt\n Variable Names do not match!"))
    }

    categories_included <- readLines(file.path(where_to_find,
                                               "included.categories"))
    categories_included <- lapply(categories_included,
                                  function(x){return(unlist(strsplit(x = x,
                                                                     split = ", ")))})
    names(categories_included) <- lapply(categories_included,
                                         function(x){x[1]})
    categories_included        <- lapply(categories_included,
                                         function(x){x[-1]})

    res$models <- lapply(X = models,
                         FUN = function(x, categories){
                           x1 <- length(categories[[x[1]]])
                           x2 <- length(categories[[x[2]]])
                           res <- matrix(data = x[-(1:2)],
                                         nrow = x1,
                                         ncol = x2,
                                         byrow = TRUE,
                                         dimnames = list(categories[[x[1]]],
                                                         categories[[x[2]]]))
                           return(res)},
                         categories = categories_included
    )
    res <- res[which(res$p.value < getOption("mdrdist_cutting_p")), ]
    return(res)
  } else{
    warning(sprintf("Corruppt files in directory %s.\n  ",
                    where_to_find))
    return(character(0))
  }
}
