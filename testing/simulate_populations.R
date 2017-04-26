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
                                        sample_size = 500,
                                        maf = .2,
                                        n.snp = 20)
Ynew <- replicate(n = nrow(Xnew), expr = c())
for (i in 1:nrow(Xnew)){
  if (Xnew$first_pop_status[i]){
    Ynew[[i]] <- c(Ynew[[i]], 1)
  }
  if (Xnew$second_pop_status[i]){
    Ynew[[i]] <- c(Ynew[[i]], 2)
  }
}
Xnew <- Xnew[, 1:20]


Xold <- generate_samples_old_simulation(sample_size = 3000,
                                        data = penetrances,
                                        first_pop = "strong_dominant",
                                        second_pop = "strong_dominant")
Yold <- Xold$classification
Xold <- Xold$designmatrix[]