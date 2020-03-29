#' @title Calls the acq2txt command (python bioread package) to convert an acq file to txt
#' @description This is an R wrapper around the acq2txt python command. It also supports output directory specification and gzip'ing files
#' @param acq_files Character vector of acq files to convert to txt.
#' @param acq2txt_opts Command line options passed to acq2txt. Default: "--missing-as=."
#' @param txt_output_dir Directory for converted txt files. Will be created if it does not exist.
#' @param gzip_txt A logical (TRUE/FALSE) indicating whether to gzip txt files after conversion. Default: TRUE
#' @param acq2txt_location The full path to the acq2txt command. If not provided, this function will use the which/where command to look in the system path.
#' @details
#'   Note: the acq2hdf5 function is far faster than acq2txt and also produces a much smaller file!
#'   In general, I would recommend that command for handling Acqknowledge files
#' @importFrom checkmate assert_file_exists
#' @export
acq2txt <- function(acq_files, acq2txt_opts="--missing-as=.", txt_output_dir=NULL, gzip_txt=TRUE, acq2txt_location=NULL) {
  sapply(acq_files, assert_file_exists) #verify that all acq files exist

  #if opts includes --outfile=<file>, convert to -o <file> approach to allow parser below to work
  acq2txt_opts <- sub("--outfile=('|\")*([^\"'])+('|\")*", "-o \\1\\2\\3", acq2txt_opts, perl=TRUE)

  #override internal use of -o flag in acq2txt to allow for internal R move commands
  #moreover, -o needs to be specified on a file-by-file basis, but this function is intended to support a vector of files
  has_o <- grepl("-o\\b", acq2txt_opts, perl=TRUE) #look for hyphen o followed by a word boundary
  if (has_o) {
    arg_split <- strsplit(acq2txt_opts, "\\s+", perl=TRUE)[[1]]
    o_pos <- which(arg_split=="-o")
    stopifnot(length(o_pos)==1L)
    f_pos <- o_pos + 1
    if (substring(arg_split[f_pos], 1, 1) %in% c("'", "\"")) {
      f_end <- grep("\"", arg_split, fixed=TRUE)
      f_end <- f_end[f_end > f_pos]
      stopifnot(length(f_end) > 0L)
      if (is.null(txt_output_dir)) {
        txt_output_dir <- dirname(paste(arg_split[f_pos:f_end], collapse=" ")) #retain the directory specified in -o
        message("Using output directory specified in -o: ", txt_output_dir)
      } else {
        warning("Ignoring -o flag to acq2txt because txt_output_dir provided as function argument.")
      }

      f_pos <- f_end[1] #set end of file position to first quote after the opening quote
    }
    arg_split <- arg_split[-1*o_pos:f_pos] #remove the -o flag and the subsequent path
    acq2txt_opts <- paste(arg_split, collapse=" ")
  }

  if (is.null(txt_output_dir)) { txt_output_dir <- getwd() } #output to current working directory if not otherwise specified
  if (!dir.exists(txt_output_dir)[1L]) { dir.create(txt_output_dir, showWarnings = FALSE) }

  # detect operating system
  os_type <- .Platform$OS.type

  # retrieve the path to the acq2txt utility
  if (is.null(acq2txt_location)) {
    if (os_type == "unix") {
      acq2txt_location <- system2("which", "acq2txt", stdout = TRUE)[1]
    } else if (os_type == "") {
      acq2txt_location <- system2("where", "acq2txt.exe", stdout = TRUE)[1] #untested on Windows
    } else {
      stop("Cannot determine location of acq2txt utility on OS: ", os_type)
    }
  } else {
    assert_file_exists(acq2txt_location)
  }

  #verify that the acq2txt command exists and is executable
  if(!grepl("acq2txt", acq2txt_location)){
    stop("You must install the bioread python package and add the acq2txt utility to PATH before calling this function.")
  } else {
    # Check if the file exists and is executable
    # base::file.access() returns values 0 for success and -1 for failure
    if(unname(file.access(acq2txt_location, mode=0)) != 0) { stop(paste(acq2txt_location, "... File does not exist.", sep="\n")) }
    if(unname(file.access(acq2txt_location, mode=1)) != 0) { stop(paste(acq2txt_location, "... File is not executable.", sep="\n")) }
  }

  #run acq2txt for each file
  txt_files <- rep(NA_character_, length(acq_files))
  for (ff in 1:length(acq_files)) {
    #specify output location
    output_fname <- file.path(txt_output_dir, sub("\\.acq\\b", ".txt", basename(acq_files[ff])))

    quote_type <- ifelse(os_type=="windows", "cmd2", "sh")
    ret <- system2(acq2txt_location, args = paste(acq2txt_opts, "-o", shQuote(output_fname, type=quote_type), shQuote(acq_files[ff], type=quote_type)), stdout = "")
    if (ret != 0) {
      warning("acq2txt returned non-zero exit status: ", ret, " for file: ", acq_files[ff])
      next #skip to next file
    }

    if (gzip_txt) { #gzip txt file if requested
      system(paste("gzip", shQuote(output_fname)))
      output_fname <- paste0(output_fname, ".gz")
    }

    txt_files[ff] <- output_fname
  }

  return(txt_files)

}

#test <- acq2txt("/Users/michael/Data_Analysis/neuromap/s4_behav_data/physio/data/nmap016/nmap016.acq", txt_output_dir = "/Users/michael/temp_acq")

#super fast for reading acq files
#' @importFrom data.table fread


