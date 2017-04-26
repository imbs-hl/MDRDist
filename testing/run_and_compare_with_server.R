set.seed(1235)

library(MDRDist)

workdir   <- path.expand("~/Dokumente/MBMDR-Methodenvergleich/SANDBOX/HELLOWORLD/")
scriptdir <- path.expand("~/Dokumente/MBMDR-Methodenvergleich/")

source(file.path(scriptdir, "generate_population_functions.R"))
source(file.path(scriptdir, "simulate_populations.R"))


foo <- MDRDist::mdr_dist(data = Xnew, working_dir = workdir, n_rounds = 20)
resultat <- cluster_distance_ratio(real_classes = Ynew, distance_matrix = foo$dist)
