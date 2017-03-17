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