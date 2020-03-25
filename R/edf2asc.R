##' @title Call SRR "edf2asc" command line utility to do some work.
##'
##' @description A convenience wrapper around the SR Research edf2asc file conversion utility.
##'   Adapted by Michael Hallquist to handle options as an argument, support gzip compression, and support an output directory
##'
##' @details Call SR Research "edf2asc" command line utility to convert *edf files to *asc
##'     files. Each *asc file is placed in the same directory as the *edf file that it is derived
##'     from. Existing *asc files will NOT be over-written by default, because that is the default
##'     for the SRR edf2asc utility.
##'
##'     To prepare edf2asc() to work with the edf2asc command line utility provided by SR Research,
##'     follow these steps:
##'     \itemize{
##'         \item{Mac OS X
##'             \enumerate{
##'                 \item{Download and install the Eyelink Developers Kit from SR Research:
##'                     \url{https://www.sr-support.com/forum/downloads/eyelink-display-software/45-eyelink-developers-kit-for-mac-os-x-mac-os-x-display-software?15-EyeLink-Developers-Kit-for-Mac-OS-X=}
##'                     \item Documentation for using edf2asc utility is in the EyeLink 1000 User
##'                     Manual, section 4.8 "Using ASC files".
##'                 }
##'                 \item Identify the path to the edf2asc utility; something like
##'                 "/Applications/Eyelink/EDF_Access_API/Example"
##'                 \item In R, check if the path to the edf2asc utility is in the R environment by
##'                 running Sys.getenv("PATH"). If yes, you may skip the following steps. If not,
##'                 do the following.
##'                 \item Open (or create) a file called ".Renviron" in the home directory (applied
##'                 system-wise) or current directory (applied project-wise)
##'                 \item Add the path to the edf2asc utility to PATH in ".Renviron", followed by
##'                 the output from Sys.getenv("PATH"); for example,
##'                 PATH="/Applications/Eyelink/EDF_Access_API/Example:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin"
##'                 \item You may need to restart R for the changes to apply.
##'             }
##'         }
##'         \item{Windows
##'             \enumerate{
##'                 \item{Download and install the Eyelink Developers Kit from SR Research:
##'                     \url{https://www.sr-support.com/forum/downloads/eyelink-display-software/39-eyelink-developers-kit-for-windows-windows-display-software?6-EyeLink-Developers-Kit-for-Windows-=}
##'                     \item Documentation for using edf2asc utility is in the EyeLink 1000 User
##'                     Manual, section 4.8 "Using ASC files".
##'                 }
##'                 \item Identify the path to the edf2asc utility; something like "C:/Program Files
##'                 (x86)/SR Research/Eyelink/EDF_Access_API/Example"
##'                 \item Make sure "edfapi.dll" and "edfapi.lib" are somewhere on the path, for
##'                 example "C:\\Windows\\System32" (see /path/to/EDF_Access_API/readme.txt for more
##'                 detail)
##'                 \item In R, check if the path to the edf2asc utility is in the R environment by
##'                 running Sys.getenv("PATH"). If yes, you may skip the following steps. If not, do
##'                 the following.
##'                 \item Go to Control Panel > System and Security > Advanced System Settings >
##'                 Environmental Variables...
##'                 \item{Under System Variables, choose Path, click Edit..., add the path to the
##'                 edf2asc utility to the list, and click OK (For more detail, see
##'                     \url{https://www.howtogeek.com/118594/how-to-edit-your-system-path-for-easy-command-line-access/})}
##'                 \item You may need to restart R for the changes to apply.
##'             }
##'         }
##'     }
##'
##'     Before calling the utility, this function will check to see that the specified file exists
##'     and is executable. However, if the selected version of the edf2asc executable is in some way
##'     incompatible with your platform, then this function will fail with a cryptic error. The best
##'     way to guard against this is to check that your edf2asc executable file works as expected
##'     from the command line before attempting to use it from within FDBeye.
##'
##'     The function edf2asc() also checks getOption("FDBeye_edf2asc_opts"). If this option exists,
##'     it should be a valid string of command line options to pass to the SRR edf2asc utility
##'     (e.g., "-y -ns"). See the SRR documentation for details. We recommend to use the "-y" option
##'     to overwrite existing *asc files; otherwise, edf2asc() may not work properly.
##'
##'     In addition to creating the requested *asc files, this function will write a log file
##'     ('edf2asc.log') of messages captured from the stdout of SRR edf2asc utility and place it in
##'     the current working directory.
##'
##' @param edf_files Character vector of *edf file names to be converted. File names should include
##'     paths relative to the current working directory, or fully qualified paths.
##' @param edf2asc_opts Options passed to edf2asc command
##' @return Called for the side effect of converting SRR *edf files to *asc files. Returns a
##'     character vector listing output files (*asc files).
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @author Monica Li \email{monica.yc.li@@gmail.com}
##' @export
##' @examples
##' \dontrun{
##' fin <- list.files(".", pattern="edf$", recursive=TRUE)
##' fout <- edf2asc(fin)
##' fout
##' }

edf2asc <- function(edf_files, edf2asc_opts="-y", asc_output_dir=NULL, gzip_asc=TRUE) {
  # detect operating system
  info <- sessionInfo()

  #override internal use of -p flag in edf2asc to allow for internal R move commands
  has_p <- grepl("-p\\b", edf2asc_opts, perl=TRUE) #look for hyphen p followed by a word boundary
  if (has_p) {
    arg_split <- strsplit(edf2asc_opts, "\\s+", perl=TRUE)[[1]]
    p_pos <- which(arg_split=="-p")
    stopifnot(length(p_pos)==1L)
    f_pos <- p_pos + 1
    if (substring(arg_split[f_pos], 1, 1) %in% c("'", "\"")) {
      f_end <- grep("\"", arg_split, fixed=TRUE)
      f_end <- f_end[f_end > f_pos]
      stopifnot(length(f_end) > 0L)
      if (is.null(asc_output_dir)) {
        asc_output_dir <- paste(arg_split[f_pos:f_end], collapse=" ")
      } else {
        warning("Ignoring -p flag to edf2asc because asc_output_dir provided as function argument.")
      }

      f_pos <- f_end[1] #set end of file position to first quote after the opening quote
    }
    arg_split <- arg_split[-1*p_pos:f_pos] #remove the -p flag and the subsequent path
    edf2asc_opts <- paste(arg_split, collapse=" ")
  }

  # retrieve the path to the edf2asc utility
  if (grepl('mac', info$running, ignore.case = TRUE)) {
    edf2asc_dir <- system2("which", "edf2asc", stdout = TRUE)
    exe <- edf2asc_dir[1]
  } else if (grepl('win', info$running, ignore.case = TRUE)) {
    edf2asc_dir <- system2("where", "edf2asc.exe", stdout = TRUE)
    exe <- edf2asc_dir[1]
  } else {
    stop("Only Mac OSX and Windows are supported currently.")
  }

  if(!grepl("edf2asc", exe)){
    stop("You must add the edf2asc utility to PATH before calling this function.")
  } else {
    # Check if the file exists and is executable
    # base::file.access() returns values 0 for success and -1 for failure
    if(unname(file.access(exe, mode=0)) != 0) { stop(paste(exe, "... File does not exist.", sep="\n")) }
    if(unname(file.access(exe, mode=1)) != 0) { stop(paste(exe, "... File is not executable.", sep="\n")) }
  }

  if(!grepl("-y", edf2asc_opts)) {
    warning(paste("Including option -y in edf2asc_opts is recommended to overwrite existing files.",
                  "Otherwise, program might not run properly.", sep="\n"))
  }

  # check if any file in edf_files is missing
  for (ff in edf_files) {
    if(!file.exists(ff)) {
      warning(paste("The following file does not exist: ", ff))
      edf_files <- edf_files[edf_files != ff]
    }
  }

  for (ff in edf_files) {
    if (grepl('mac|win', info$running, ignore.case = TRUE)) {
      ## see R function shQuote() for help building the command line string.
      log <- system2(exe,
                     args = shQuote(paste(edf2asc_opts, ff), type = "cmd2"),
                     stdout = TRUE)
    } else {
      stop("Only Mac OSX and Windows are supported currently.")
    }

    if(exists("logfile")) logfile <- c(logfile, log)
    else logfile <- log

    asc_file <- sub("\\.edf$", ".asc", ff)

    if (gzip_asc) { system(paste("gzip", asc_file)) } #gzip asc file if requested
  }

  ## should wrap this in a 'try' block.
  logfile <- logfile[-grep("^Processed", logfile)]

  log_dir <- ifelse(is.null(asc_output_dir), getwd(), asc_output_dir)
  h <- file(file.path(log_dir, "edf2asc.log"), "wb")
  cat(logfile, file=h, sep="\n")
  close(h)

  ext <- ifelse(gzip_asc, ".asc.gz", ".asc")
  asc_files <- gsub("\\.edf$", ext, edf_files)

  #move files to output directory, if requested
  if (!is.null(asc_output_dir)) {
    for (ff in asc_files) {
      #file.rename(ff, file.path(asc_output_dir, basename(ff)))  #breaks down when we need to move across devices/drives
      file.copy(ff, file.path(asc_output_dir, basename(ff)))
      file.remove(ff)
    }

    asc_files <- file.path(asc_output_dir, basename(asc_files))
  }

  return(asc_files)
}

# test code
# efiles <- list.files("~/Box/s3_behav_data/neighborhood/eye/data/raw", pattern=".edf", full.names=T)
# abc <- edf2asc(efiles, asc_output_dir = "/Users/michael")
