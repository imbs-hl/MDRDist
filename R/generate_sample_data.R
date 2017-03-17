#' Generate genetic sample data
#'
#' @description This function generates a data set that could be used for testing the algorithms.
#'
#' @details This function generates a set of unclassified data and a vector to evaluate the clustering.
#' The data should be interpreted as two hidden clusters. For each cluster \code{n=2000} individuals are generated,
#' of which some become deseased. The disease appears, if
#' \itemize{
#' \item{for the first cluster with chance 80\%, if snpA > 1 and snpB > 1, with chance 5\% otherwise and}
#' \item{for the second cluster with chance 80\%, if snpC > 1 and snpD > 1, with chance 5\% otherwise.}
#' }
#'
#'
#' @return The function returns a list
#' \item{\code{$data}}{A \code{data.frame} with 20 observed markers and about 630 individuals. The Markers are called
#' \itemize{
#' \item{\code{snpA, snpB, snpC, snpD} with MAF 0.2}
#' \item{\code{noise1 to noise4} with MAF 0.1}
#' \item{\code{noise5 to noise12} with MAF 0.2}
#' \item{\code{noise13 to noise16} with MAF 0.3} }}
#' \item{\code{$classification}}{a vector containing for each observation, in which cluster it has been generated}
#'
#' @export
#'
#'
generate_sample_data <- function(){
  sample_size <- 2000

  Data1 <- simulate_data1(sample_size = sample_size)
  Data1$Class <- 0

  Data2 <- simulate_data2(sample_size = sample_size)
  Data2$Class <- 1

  Data <- rbind(Data1, Data2)
  Data <- Data[which(Data$status == 1), !(colnames(Data) %in% c("status"))]

  return(list(data = as.data.frame(Data[,  !(colnames(Data) %in% c("Class"))]),
              classification = Data[, "Class"]))
}

simulate_data1 <- function(sample_size){
  A = matrix(
    data = c(0.05, 0.05, 0.05,
             0.05, 0.80, 0.80,
             0.05, 0.80, 0.80),
    nrow=3,                 # number of rows
    ncol=3,                 # number of columns
    byrow = TRUE)           # fill matrix by rows

  X <- simulate_genotypes(sample_size = sample_size)
  Y <- apply(X = data.frame(x=X$snpA,
                            y=X$snpB),
            MARGIN = 1,
            FUN = function(x){
              A[x[1] + 0,
                x[2] + 0]
              })
  X$status <- rbinom(n = sample_size,
                     prob = Y,
                     size = 1)
  return(X)
}

simulate_data2 <- function(sample_size){
  B = matrix(
    data = c(0.05, 0.05, 0.05,
             0.05, 0.80, 0.80,
             0.05, 0.80, 0.80),
    nrow=3,                 # number of rows
    ncol=3,                 # number of columns
    byrow = TRUE)           # fill matrix by rows

  X <- simulate_genotypes(sample_size = sample_size)
  Y <- apply(X = data.frame(x=X$snpC,
                            y=X$snpD),
             MARGIN = 1,
             FUN = function(x){
               B[x[1] + 0,
                 x[2] + 0]
               })
  X$status <- rbinom(n = sample_size,
                     prob = Y,
                     size = 1)
  return(X)
}

# Function simulating genotype data (additive model)
simulate_genotypes <- function(sample_size){
  X <- data.frame(snpA = sample(c(1, 2, 3),
                                replace = TRUE,
                                size = sample_size,
                                prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  snpB = sample(c(1, 2, 3),
                                replace = TRUE,
                                size = sample_size,
                                prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  snpC = sample(c(1, 2, 3),
                                replace = TRUE, size = sample_size,
                                prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  snpD = sample(c(1, 2, 3),
                                replace = TRUE, size = sample_size,
                                prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise1 = sample(c(1, 2, 3),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.81, 0.18, 0.01)), # MAF is 0.1
                  noise2 = sample(c(1, 2, 3),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.81, 0.18, 0.01)), # MAF is 0.1
                  noise3 = sample(c(1, 2, 3),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.81, 0.18, 0.01)), # MAF is 0.1
                  noise4 = sample(c(1, 2, 3),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.81, 0.18, 0.01)), # MAF is 0.1
                  noise5 = sample(c(1, 2, 3),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise6 = sample(c(1, 2, 3),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise7 = sample(c(1, 2, 3),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise8 = sample(c(1, 2, 3),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise9 = sample(c(1, 2, 3),
                                  replace = TRUE,
                                  size = sample_size,
                                  prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise10 = sample(c(1, 2, 3),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise11 = sample(c(1, 2, 3),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise12 = sample(c(1, 2, 3),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.64, 0.32, 0.04)), # MAF is 0.2
                  noise13 = sample(c(1, 2, 3),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.49, 0.42, 0.09)), # MAF is 0.3
                  noise14 = sample(c(1, 2, 3),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.49, 0.42, 0.09)), # MAF is 0.3
                  noise15 = sample(c(1, 2, 3),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.49, 0.42, 0.09)), # MAF is 0.3
                  noise16 = sample(c(1, 2, 3),
                                   replace = TRUE,
                                   size = sample_size,
                                   prob = c(0.49, 0.42, 0.09))  # MAF is 0.3
                  )
  return(X)
}
