
#########
# ----------- new way
#########

generate_samples_new_simulation <- function(data, job, first_pop, second_pop, sample_size, maf, n.snp = 20) {

  X <- simulate_snps(n.obs = 30000, n.snp = n.snp,
                     maf = c(rep(maf, 4), runif(n.snp-4, 0.1, 0.4)))

  first_pop_penetrances = matrix(
    data = data[[first_pop]],
    nrow=3,                 # number of rows
    ncol=3,                 # number of columns
    byrow = TRUE)           # fill matrix by rows
  second_pop_penetrances = matrix(
    data = data[[second_pop]],
    nrow=3,                 # number of rows
    ncol=3,                 # number of columns
    byrow = TRUE)           # fill matrix by rows

  Y <- apply(X = data.frame(x=X[, 1],
                            y=X[, 2]),
             MARGIN = 1,
             FUN = function(x){
               first_pop_penetrances[x[1] + 1,
                                     x[2] + 1]
             })
  X$first_pop_status <- rbinom(n = length(Y),
                               prob = Y,
                               size = 1)

  Y <- apply(X = data.frame(x=X[, 3],
                            y=X[, 4]),
             MARGIN = 1,
             FUN = function(x){
               second_pop_penetrances[x[1] + 1,
                                      x[2] + 1]
             })
  X$second_pop_status <- rbinom(n = length(Y),
                                prob = Y,
                                size = 1)

  X$status <- X$first_pop_status | X$second_pop_status

  return(X[X$status == 1, ][1:sample_size, ])

}


## ================================================================= ##
## Simulate SNP data ----
## ================================================================= ##
simulate_snps <- function(n.obs = 100, n.snp = 100, maf = 0.25) {

  assertions <- checkmate::makeAssertCollection()

  checkmate::assertCount(n.obs, na.ok = FALSE, null.ok = FALSE, positive = TRUE,
                         add = assertions)
  checkmate::assertCount(n.snp, na.ok = FALSE, null.ok = FALSE, positive = TRUE,
                         add = assertions)

  checkmate::assertNumeric(maf, lower = 0, upper = 0.5, min.len = 1, max.len = n.snp,
                           finite = TRUE,
                           any.missing = FALSE,
                           null.ok = FALSE,
                           add = assertions)

  checkmate::reportAssertions(assertions)

  if (length(maf) == 1) {
    maf <- rep(maf, n.snp)
  }

  ## Create SNP matrix
  X <- data.frame(sapply(maf, function(x) {
    sample(c(0, 1, 2), n.obs, replace = TRUE,
           prob = c((1-x)^2, 2*(1-x)*x, x^2))
  }))
  colnames(X) <- sprintf(paste0("SNP%0", nchar(n.snp), "d"), 1:n.snp)
  rownames(X) <- sprintf(paste0("ID%0", nchar(n.obs), "d"), 1:n.obs)

  return(X)

}



#########
# ----------- old way
#########


generate_samples_old_simulation <- function(sample_size, data, first_pop, second_pop){

  data1 <- simulate_data1(sample_size = sample_size, data = data, first_pop = first_pop)
  data1$Class <- 1
  data2 <- simulate_data2(sample_size = sample_size, data = data, second_pop = second_pop)
  data2$Class <- 2
  Data <- rbind(data1, data2)
  Data <- Data[which(Data$status == 1), !(colnames(Data) %in% c("status"))]

  return(list(designmatrix = Data[,  !(colnames(Data) %in% c("Class"))],
              classification = Data[, "Class"]))
}

simulate_data1 <- function(sample_size, data, first_pop){
  A = matrix(
    data = data[[first_pop]], # the data elements 
    nrow=3,                   # number of rows 
    ncol=3,                   # number of columns 
    byrow = TRUE)             # fill matrix by rows
  X <- simulate_genotypes(sample_size = sample_size)
  Y = apply(data.frame(x=X$snpA, y=X$snpB), 1, function(x){A[x[1] + 1, x[2] + 1]})
  X$status <- rbinom(sample_size, prob = Y, size = 1)
  return(X)
}

simulate_data2 <- function(sample_size, data, second_pop){
  B = matrix(
    data = data[[second_pop]],  # the data elements
    nrow=3,                     # number of rows 
    ncol=3,                     # number of columns 
    byrow = TRUE)               # fill matrix by rows
  X <- simulate_genotypes(sample_size = sample_size)
  Y = apply(data.frame(x=X$snpC, y=X$snpD), 1, function(x){B[x[1] + 1, x[2] + 1]})
  X$status <- rbinom(sample_size, prob = Y, size = 1)
  return(X)
}


### function for simulating random noise snp
#randGenProb <- function() {
#  maf <- runif(1, min = .05, max = 0.5)
#  c( (1 - maf) ^ 2, 2 * maf * (1 - maf), maf ^ 2)
#}

# Function simulating genotype data (additive model)
simulate_genotypes <- function(sample_size){
  X <- data.frame(snpA = sample(c(0, 1, 2),
                                replace = TRUE,
                                size = sample_size,
                                prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  snpB = sample(c(0, 1, 2),
                                replace = TRUE,
                                size = sample_size,
                                prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  snpC = sample(c(0, 1, 2),
                                replace = TRUE, size = sample_size,
                                prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  snpD = sample(c(0, 1, 2),
                                replace = TRUE, size = sample_size,
                                prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise1 = sample(c(0, 1, 2),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.81, 0.18, 0.01)), # MAF is 0.2
                  noise2 = sample(c(0, 1, 2),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.81, 0.18, 0.01)), # MAF is 0.2
                  noise3 = sample(c(0, 1, 2),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.81, 0.18, 0.01)), # MAF is 0.2
                  noise4 = sample(c(0, 1, 2),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.81, 0.18, 0.01)), # MAF is 0.2
                  noise5 = sample(c(0, 1, 2),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise6 = sample(c(0, 1, 2),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise7 = sample(c(0, 1, 2),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise8 = sample(c(0, 1, 2),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise9 = sample(c(0, 1, 2),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise10 = sample(c(0, 1, 2),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise11 = sample(c(0, 1, 2),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise12 = sample(c(0, 1, 2),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise13 = sample(c(0, 1, 2),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.49, 0.42, 0.09)), # MAF is 0.2
                  noise14 = sample(c(0, 1, 2),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.49, 0.42, 0.09)), # MAF is 0.2
                  noise15 = sample(c(0, 1, 2),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.49, 0.42, 0.09)), # MAF is 0.2
                  noise16 = sample(c(0, 1, 2),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.49, 0.42, 0.09)) # MAF is 0.2
  )
  return(X)
}

#####
#--------- cleanup
#####


getOriginClasses <- function(X){
  x1 <- X[1] & X[2]
  x2 <- X[3] & X[4]
  x3 <- !(x1|x2)
  
  return(as.numeric(x3) + 
           2 * as.numeric(x1) + 
           3 * as.numeric(x2))
}

