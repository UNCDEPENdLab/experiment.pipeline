#' @title Calls the acq2hdf5 command (python bioread package) to convert an acq file to an hdf5 file
#' @description This is an R wrapper around the acq2hdf5 python command. It also supports output directory specification
#' @param acq_files Character vector of acq files to convert to hdf5.
#' @param acq2hdf5_opts Command line options passed to acq2hdf5. Default: "--missing-as=."
#' @param hdf5_output_dir Directory for converted hdf5 files. Will be created if it does not exist.
#' @param gzip_hdf5 A logical (TRUE/FALSE) indicating whether to gzip hdf5 files after conversion. Default: TRUE
#' @param acq2hdf5_location The full path to the acq2hdf5 command. If not provided, this function will use the which/where command to look in the system path.
#' @details
#'   Note: the acq2hdf5 function is far faster than acq2hdf5 and also produces a much smaller file!
#'   In general, I would recommend that command for handling Acqknowledge files
#' @importFrom checkmate assert_file_exists
#' @export
acq2hdf5 <- function(acq_files, acq2hdf5_opts="--compress=gzip", hdf5_output_dir=NULL, acq2hdf5_location=NULL) {

  sapply(acq_files, assert_file_exists) #verify that all acq files exist
  acq_files <- sapply(acq_files, normalizePath) #ensure that paths are fully understandable by shell (e.g., ~/ won't work)

  if (is.null(hdf5_output_dir)) { hdf5_output_dir <- getwd() } #output to current working directory if not otherwise specified
  if (!dir.exists(hdf5_output_dir)[1L]) { dir.create(hdf5_output_dir, showWarnings = FALSE) }

  hdf5_output_dir <- normalizePath(hdf5_output_dir) #ensure that absolute path is passed to bioread function

  # detect operating system
  os_type <- .Platform$OS.type

  # retrieve the path to the acq2hdf5 utility
  if (is.null(acq2hdf5_location)) {
    if (os_type == "unix") {
      acq2hdf5_location <- system2("which", "acq2hdf5", stdout = TRUE)[1]
    } else if (os_type == "") {
      acq2hdf5_location <- system2("where", "acq2hdf5.exe", stdout = TRUE)[1] #untested on Windows
    } else {
      stop("Cannot determine location of acq2hdf5 utility on OS: ", os_type)
    }
  } else {
    assert_file_exists(acq2hdf5_location)
  }

  #verify that the acq2hdf5 command exists and is executable
  if(!grepl("acq2hdf5", acq2hdf5_location)){
    stop("You must install the bioread python package and add the acq2hdf5 utility to PATH before calling this function.")
  } else {
    # Check if the file exists and is executable
    # base::file.access() returns values 0 for success and -1 for failure
    if(unname(file.access(acq2hdf5_location, mode=0)) != 0) { stop(paste(acq2hdf5_location, "... File does not exist.", sep="\n")) }
    if(unname(file.access(acq2hdf5_location, mode=1)) != 0) { stop(paste(acq2hdf5_location, "... File is not executable.", sep="\n")) }

    acq2hdf5_location <- normalizePath(acq2hdf5_location) #ensure that ~/ is handled gracefully
  }

  #run acq2hdf5 for each file
  hdf5_files <- rep(NA_character_, length(acq_files))
  for (ff in 1:length(acq_files)) {
    #specify output location
    output_fname <- file.path(hdf5_output_dir, sub("\\.acq\\b", ".h5", basename(acq_files[ff])))

    quote_type <- ifelse(os_type=="windows", "cmd2", "sh")
    ret <- system2(acq2hdf5_location, args = paste(acq2hdf5_opts, shQuote(acq_files[ff], type=quote_type), shQuote(output_fname, type=quote_type)), stdout = "")
    if (ret != 0) {
      warning("acq2hdf5 returned non-zero exit status: ", ret, " for file: ", acq_files[ff])
      next #skip to next file
    }

    hdf5_files[ff] <- output_fname
  }

  return(hdf5_files)

}

# test <- acq2hdf5("/Users/mnh5174/Data_Analysis/neuromap/s4_behav_data/physio/data/nmap016/nmap016.acq",
#                  hdf5_output_dir = "/Users/mnh5174/temp_acq",
#                  acq2hdf5_location = "/Users/mnh5174/Library/Python/3.7/bin/acq2hdf5")

