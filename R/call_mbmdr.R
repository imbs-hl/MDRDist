#' Wrapper for MBMDR-4.4.1  (up to the extern function call)
#'
#' @description The c++-software MBMDR-4.4.1 expects a specific data format,
#'    which is read in via harddrive.
#'    This function stores the data and starts the programme from shell.
#'
#' @param formula selecting rows from data
#' @param data a data.frame with observations (in the rows), with the columns
#'    containing categories, that are encoded as positive integers. Missing
#'    values are encoded as -9 !
#' @param working_dir some empty directory to communicate with MBMDR-4.4.1 and
#'    to store results.
#'
#' @return the path to a (random looking) subdirectory of working_dir,
#'    in which files were stored
#'
#' @import checkmate
#'
#' @export
#'

call_mbmdr <- function(formula, data, working_dir){
  # This function
  # ... checks input
  # ... saves data for mbmdr and some additional data to harddrive
  # ... runs mbmdr
  # ... returns directory, to which mbmdr-files are saved

  # check input for correct data formats
  checkmate::assert_data_frame(x = data, types = "numeric")
  checkmate::assertClass(formula, "formula")
  checkmate::assert_directory_exists(working_dir)

  #checkmate::assertInteger()
  # check data is integer
  # check data is positive
  # check status is {0,1}

  work_dir <- tempfile(pattern = "",
                       tmpdir = working_dir)
  dir.create(path = work_dir,
             recursive = TRUE)

  wait_for_dir(work_dir)

  input_data <- model.frame(formula, data)
  rowname_memory <- row.names(input_data)

  used_categories <- apply(input_data, MARGIN = 2, function(x){
    X <- c(names(table(x)))
    return(toString(X))
  })

  used_categories <- paste(colnames(input_data), unlist(used_categories), sep = ", ")


  input_data <- as.data.frame(lapply(input_data,
                                     function(x){
                                       replace(x,
                                               which(is.na(x)),
                                               -9)
                                     }))
  rownames(input_data) <- rowname_memory

  write.table(x = input_data,
              file = file.path(work_dir,
                               "input.mbmdr"),
              sep = " ",
              col.names = TRUE,
              row.names = FALSE)

  write.table(x = rowname_memory,
              file = file.path(work_dir,
                               "generating_sample"),
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE)

  writeLines(text = unlist(used_categories),
             con = file.path(work_dir,
                             "included.categories"))



  system2(path.expand(getOption("mdrdist_command")),
          args =
            c(
              "--binary",
              "-p", getOption("mdrdist_p"),
              "-n", "100",
              "-x", getOption("mdrdist_x"),
              "-a", getOption("mdrdist_a"),
              "-m", getOption("mdrdist_m"),
              "-d", "2D",
              "-o", paste0("\"",
                           file.path(work_dir,
                                     "output.table"),
                           "\""),
              "-o2", paste0("\"",
                            file.path(work_dir,
                                      "output.models"),
                            "\""),
              "-v", "SHORT",
              file.path(work_dir,
                        "input.mbmdr")),
          stdout = file.path(work_dir,
                             "mbmdr.log"),
          stderr = file.path(work_dir,
                             "mbmdr.err"))
  wait_for_file(File = file.path(work_dir,
                                 "output.models"))
  wait_for_file(File = file.path(work_dir,
                                 "output.table"))
  mbmdr_models_exist <- checkmate::testFileExists(x = file.path(work_dir,
                                                                "output.models"))
  mbmdr_table_exist  <- checkmate::testFileExists(x = file.path(work_dir,
                                                                "output.table"))
  if(mbmdr_table_exist & mbmdr_models_exist){
    writeLines(text = "MB-MDR completed successfully.",
               con = file.path(work_dir,
                               "CHECK"))
  } else{
    warning(sprintf("Somehow, MB-MDR went wrong! See logs in %s.",
                    work_dir))
  }
  return(work_dir)
}
