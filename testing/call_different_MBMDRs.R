library(MDRDist)
library(mbmdRdist)
library(mbmdRwrap)


Xold_old <- mbmdRdist::dist_unlabled(list_of_affecteds = Xold, 
                                     rounds = 20, 
                                     working_dir = "~/Dokumente/MBMDR-Methodenvergleich/MBMDR_old_old/", 
                                     mbmdr_version = "4.4", 
                                     mbmdr_command = "mbmdr",
                                     cleanup = FALSE)

Xold_new <- mbmdRdist::dist_unlabled(list_of_affecteds = Xnew, 
                                     rounds = 20, 
                                     working_dir = "~/Dokumente/MBMDR-Methodenvergleich/MBMDR_old_new/", 
                                     mbmdr_version = "4.4", 
                                     mbmdr_command = "mbmdr",
                                     cleanup = FALSE)

Xnew_old <- MDRDist::mdr_dist(data = Xold, 
                              working_dir = "~/Dokumente/MBMDR-Methodenvergleich/MBMDR_new_old/", 
                              n_rounds = 20, 
                              use_existing_models = FALSE, 
                              evaluate_models = TRUE)

Xnew_new <- MDRDist::mdr_dist(data = Xnew, 
                              working_dir = "~/Dokumente/MBMDR-Methodenvergleich/MBMDR_new_new/", 
                              n_rounds = 20, 
                              use_existing_models = FALSE, 
                              evaluate_models = TRUE)
