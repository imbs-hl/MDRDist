library(checkmate)

library(MDRDist)
library(mbmdRdist)
library(mbmdRwrap)
source("~/Dokumente/MBMDR-Methodenvergleich/generate_population_functions.R")

first_pop_value <- "strong_dominant"
second_pop_value <- "strong_dominant"

working_dir <- "~/Dokumente/MBMDR-Methodenvergleich/RUNS"


penetrances <- list("strong_dominant" = c(0.05, 0.05, 0.05,
                                          0.05, 0.80, 0.80,
                                          0.05, 0.80, 0.80),
                    "weak_dominant" = c(0.05, 0.05, 0.05,
                                        0.05, 0.50, 0.50,
                                        0.05, 0.50, 0.50),
                    "strong_additive" = c(0.05, 0.05, 0.05,
                                          0.05, 0.40, 0.50,
                                          0.05, 0.50, 0.80),
                    "weak_additive" = c(0.05, 0.05, 0.05,
                                        0.05, 0.25, 0.30,
                                        0.05, 0.30, 0.50),
                    "no_marginal" = c(0.492, 0.664, 0.481, # Velez Model No. 15
                                      0.642, 0.330, 0.746,
                                      0.656, 0.396, 0))

Xnew <- generate_samples_new_simulation(data = penetrances,
                                        job = "1234",
                                        first_pop = first_pop_value,
                                        second_pop = second_pop_value,
                                        sample_size = 700,
                                        maf = .2,
                                        n.snp = 20)
Ynew <- as.numeric(Xnew$first_pop_status) + 2 * as.numeric(Xnew$second_pop_status)
Xnew <- Xnew[, 1:20]
Znew <- getOriginClasses(Xnew)
print(table(Ynew, Znew))


Xold <- generate_samples_old_simulation(sample_size = 3000, 
                                        data = penetrances, 
                                        first_pop = first_pop_value, 
                                        second_pop = second_pop_value)
Yold <- Xold$classification
Xold <- Xold$designmatrix
Zold <- getOriginClasses(Xold)
print(table(Yold, Zold))


wd_oo <- file.path(working_dir, 
                   paste(first_pop_value, second_pop_value, sep = "_"),
                   "oo/")
dir.create(path = wd_oo, showWarnings = TRUE, recursive = TRUE)
wd_on <- file.path(working_dir, paste(first_pop_value, second_pop_value, sep = "_"), "on/")
dir.create(path = wd_on, showWarnings = TRUE, recursive = TRUE)
wd_no <- file.path(working_dir, paste(first_pop_value, second_pop_value, sep = "_"), "no/")
dir.create(path = wd_no, showWarnings = TRUE, recursive = TRUE)
wd_nn <- file.path(working_dir, paste(first_pop_value, second_pop_value, sep = "_"), "nn/")
dir.create(path = wd_nn, showWarnings = TRUE, recursive = TRUE)

Xold_old <- mbmdRdist::dist_unlabled(list_of_affecteds = Xold, 
                                     rounds = 20, 
                                     working_dir = wd_oo, 
                                     mbmdr_version = "4.4", 
                                     mbmdr_command = "mbmdr",
                                     cleanup = FALSE)

Xold_new <- mbmdRdist::dist_unlabled(list_of_affecteds = Xnew, 
                                     rounds = 20, 
                                     working_dir = wd_on, 
                                     mbmdr_version = "4.4", 
                                     mbmdr_command = "mbmdr",
                                     cleanup = FALSE)

Xnew_old <- MDRDist::mdr_dist(data = Xold, 
                              working_dir = wd_no, 
                              n_rounds = 20, 
                              use_existing_models = FALSE, 
                              evaluate_models = TRUE)

Xnew_new <- MDRDist::mdr_dist(data = Xnew, 
                              working_dir = wd_nn, 
                              n_rounds = 20, 
                              use_existing_models = FALSE, 
                              evaluate_models = TRUE)

frequencies <- list(Xold_old$frequency_of_interactions[1:7, 1:7], 
                    Xold_new$frequency_of_interactions[1:7, 1:7], 
                    Xnew_old$importance, 
                    Xnew_new$importance)
print(frequencies)