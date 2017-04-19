reduce_calculated_models <- function(calculated_models){

  relevant_interactions_by_index <- which(table(calculated_models$First_Marker,
                                                calculated_models$Second_Marker) > 0,
                                          arr.ind =  TRUE,
                                          useNames = FALSE)
  relevant_interactions_by_name <-
    data.frame(first = levels(calculated_models$First_Marker)[relevant_interactions_by_index[, 1]],
               second = levels(calculated_models$First_Marker)[relevant_interactions_by_index[, 2]])

  equal_interactions <- apply(X = relevant_interactions_by_name,
                              MARGIN = 1,
                              FUN = function(x, data){
                                a1 <- data$First_Marker == x[[1]]
                                b1 <- data$First_Marker == x[[2]]
                                return(which(a1 & b1))},
                              data = calculated_models)

  new_intercations <- lapply(X = equal_interactions,
                             FUN = reduce_models_to_single_HLO,
                             calculated_models = calculated_models)

  return(do.call(what = rbind, args = new_intercations))

}





reduce_models_to_single_HLO <- function(ids, calculated_models){
  currently_summarized_models <- calculated_models[ids, ]
  colnames_list <- unique(unlist(lapply(X = my_new_list$models,
                                        FUN = colnames)))
  rownames_list <- unique(unlist(lapply(X = my_new_list$models,
                                        FUN = rownames)))
  Hs <- matrix(data = 0,
               nrow = length(rownames_list),
               ncol = length(colnames_list),
               dimnames = list(colnames_list,
                               rownames_list))
  Ls <- matrix(data = 0,
               nrow = length(rownames_list),
               ncol = length(colnames_list),
               dimnames = list(colnames_list,
                               rownames_list))

  for (i in 1:nrow(currently_summarized_models)){

    Hs[rownames(currently_summarized_models$models[[i]]),
       colnames(currently_summarized_models$models[[i]])] <-
      Hs[rownames(currently_summarized_models$models[[i]]),
         colnames(currently_summarized_models$models[[i]])] +
      (currently_summarized_models$models[[i]] == "H")

    Ls[rownames(currently_summarized_models$models[[i]]),
       colnames(currently_summarized_models$models[[i]])] <-
      Ls[rownames(currently_summarized_models$models[[i]]),
         colnames(currently_summarized_models$models[[i]])] +
      (currently_summarized_models$models[[i]] == "L")
  }

  res <- matrix(data = "O",
                nrow = length(rownames_list),
                ncol = length(colnames_list),
                dimnames = list(colnames_list,
                                rownames_list))

    for (i in 1:length(rownames_list)){
    for (j in 1:length(colnames_list)){
      significant_cell <- ifelse(test = (Hs[i,j] + Ls[i,j] > 0),
                                 yes = binom.test(x = Hs[i,j],
                                                  n = Hs[i,j] + Ls[i,j],
                                                  p = 0.5)$p.value < .05,
                                 no = FALSE)
      if(significant_cell){
        res[i,j] <- ifelse(test = (Hs[i,j] >= Ls[i,j]),
                           yes = "H",
                           no = "L")
      }
    }
  }
  return_value <- data.frame(First_Marker  = currently_summarized_models$First_Marker[1],
                             Second_Marker = currently_summarized_models$Second_Marker[1],
                             count         = nrow(currently_summarized_models))
  return_value$models <- list(res)
  return(return_value)
}
