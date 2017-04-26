#' Waiting until recently written file appears
#'
#' @param File path to the file which we are waiting for
#' @param max_wait timeout until assertion will be raised, if file does not appear
#' @param timeout timestep between two attempts to look for the file
#'
#' @return nothing but certainty, that a file is callable
#'
#' @import checkmate
#'
#' @export
#'
wait_for_file <- function(File, max_wait = 30, timeout = 1){
  res <- checkmate::test_file_exists(x = File)
  timecount <- 0
  # loop: if not found, wait some time and search again
  while (!res & timecount < max_wait){
    timecount <- timecount + timeout
    Sys.sleep(timeout)
    res <- checkmate::test_file_exists(x = File)
  }
  # finally: if File has appeared, then continue. Abort otherwise.
  checkmate::assert_file_exists(x = File)
}


#' Waiting until recently created directory appears
#'
#' @param Dir path to the directory which we are waiting for
#' @param max_wait timeout until assertion will be raised, if dir does not appear
#' @param timeout timestep between two attempts to look for the dir
#'
#' @return nothing but certainty, that a directory is callable
#' @export
#'
wait_for_dir <- function(Dir, max_wait = 30, timeout = 1){
  res <- checkmate::test_directory_exists(x = Dir)
  timecount <- 0
  # loop: if not found, wait some time and search again
  while (!res & timecount < max_wait){
    timecount <- timecount + timeout
    Sys.sleep(timeout)
    res <- checkmate::test_directory_exists(x = Dir)
  }
  # finally: if File has appeared, then continue. Abort otherwise.
  checkmate::assert_directory_exists(x = Dir)
}


#' Function to calculate ratio between cluster-intern and general distances
#'
#'
#' @param real_classes the classes in which the observations are truely.
#'    This value could either be a vector with integer coded classes or a
#'    list of vectors containing multiple classes, if necessary
#' @param distance_matrix an arbitrary distance matrix, that matches in size
#'    and order of the observations with real_classes
#'
#' @return a numeric value that describes the ratio between cluster-intern and
#'    general distances in the given distance matrix. Values approximately 0 imply
#'    a good separation, values near 1 could be a sign of lacking informativity
#' @export
#'

cluster_distance_ratio <- function(real_classes, distance_matrix){
  existing_classes <- unique(unlist(real_classes))
  elements_by_classes <- lapply(X = existing_classes,
                                FUN = function(c, real_classes)
                                {
                                  res <- lapply(X = real_classes,
                                                FUN = function(x, c){c %in% x},
                                                c = c)
                                  return(unlist(res))
                                },
                                real_classes = real_classes)

  print(elements_by_classes)

  class_intern_distances <- lapply(X = elements_by_classes,
                                   FUN = function(x, distance_matrix)
                                   {return(distance_matrix[x, x])},
                                   distance_matrix = distance_matrix)

  print(class_intern_distances)
  cluster_intern_distance <- mean(unlist(class_intern_distances))
  average_distance <- mean(distance_matrix)

  return(cluster_intern_distance / average_distance)
}




