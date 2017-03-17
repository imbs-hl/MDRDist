# mbmdr_return ... read in and pasted data.frame
# data ... the data on which to calculate distance
# exclude_creating_data ... should the same data be considered for calculatin model and for calculating distance?

#' Classify Data
#'
#' @param mbmdr_return the return value of mbmdr_output with additional model-mapping-column
#' @param data the original input data as given to mbmdr_call
#'
#' @return A data.frame of class "classified_Data", where each column represents
#'  one significant interaction row from mbmdr_return and the rows name the
#'  category of each observation in the interaction table with respect to the
#'  selected model assumption.
#' @export
#'

classify_data <- function(mbmdr_return,
                          data){
  res <- apply(X = mbmdr_return,
        MARGIN = 1,
        FUN = classify_data_over_mbmdr_return,
        data = data)

  res <- as.data.frame(res)
  return(res)
}


classify_data_over_mbmdr_return <- function(mbmdr_return_row,
                                            data){
  X1 <- mbmdr_return_row$"First_Marker"
  X2 <- mbmdr_return_row$"Second_Marker"
  model <- mbmdr_return_row$"models_int"

  res <- apply(X = data,
               MARGIN = 1,
               FUN = classify_data_over_observations,
               X1 = X1,
               X2 = X2,
               model = model)

  if(getOption("mdrdist_exclude_creating_data")){
    forbidden <- readLines(con = file.path(mbmdr_return_row$"source",
                                           "generating_sample"))
    res[which(names(res) %in% forbidden)] = NA_integer_

  }

  return(res)
}


classify_data_over_observations <- function(observation_row, X1, X2, model){
  Y1 <- observation_row[as.character(X1)]
  Y1 <- as.character(Y1)
  Y2 <- observation_row[as.character(X2)]
  Y2 <- as.character(Y2)
  element_found <- (Y1 %in% rownames(model)) & (Y2 %in% colnames(model))
  if(element_found){
    res <- model[Y1, Y2]
  } else{
    res <- NA_integer_
  }
  return(res)
}

