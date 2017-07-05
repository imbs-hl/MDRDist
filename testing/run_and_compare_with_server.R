set.seed(1357)

library(MDRDist)

workdir   <- path.expand("~/Dokumente/MBMDR-Methodenvergleich/SANDBOX/HELLOWORLD2/")
scriptdir <- path.expand("~/git/MDRDist/testing/")

source(file.path(scriptdir, "generate_population_functions.R"))
source(file.path(scriptdir, "simulate_populations.R"))


foo <- MDRDist::mdr_dist(data = Xnew, working_dir = workdir, n_rounds = 100)
resultat <- cluster_distance_ratio(real_classes = Ynew, distance_matrix = foo$dist)
