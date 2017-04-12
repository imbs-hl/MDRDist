source("~/Dokumente/MBMDR-Methodenvergleich/generate_populations.R")

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
                                        first_pop = "strong_dominant",
                                        second_pop = "strong_dominant",
                                        sample_size = 250,
                                        maf = .2,
                                        n.snp = 20)
Ynew <- as.numeric(Xnew$first_pop_status) + 2 * as.numeric(Xnew$second_pop_status)
Xnew <- Xnew[, 1:20]


Xold <- generate_samples_old_simulation(sample_size = 3000, 
                                        data = penetrances, 
                                        first_pop = "strong_dominant", 
                                        second_pop = "strong_dominant")
Yold <- Xold$classification
Xold <- Xold$designmatrix[]









getOriginClasses <- function(X){
  x1 <- X[1] & X[2]
  x2 <- X[3] & X[4]
  x3 <- !(x1|x2)
  
  return(as.numeric(x3) + 
                      2 * as.numeric(x1) + 
                      3 * as.numeric(x2))
}

Znew <- getOriginClasses(Xnew)
Zold <- getOriginClasses(Xold)

table(Ynew, Znew)
table(Yold, Zold)