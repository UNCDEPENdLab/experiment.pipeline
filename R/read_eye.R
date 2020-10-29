#' general wrapper for reading eye data into the package and performing all QA and processing
read_process_eye <- function(file, yaml = NULL, gen_log = TRUE, log_dir = NULL, ...) {

  stopifnot(file.exists(file))
  if (length(file) > 1L) { stop("At present, read_process_eye is designed for one file at a time") }

  # initialize log file if requested. Otherwise will print feedback while running checks.
  ## N.B. right now this will overwrite existing files, can come back to later.
  if(gen_log){init_eyelog(log_dir)}

  ### 1. Read EDF file
  eye <- read_edf(file, keep_asc=FALSE, parse_all=TRUE, samples = TRUE)[[1]] # samples=FALSE removes read-in of raw data (significantly speeding up computation speed), but we'll keep this for now.


  ### 2. Perform basic initial validation checks and compute new variables
  eye2 <- initialize_eye(eye)

  ### 3. Parse eye messages from experiment.pipeline YAML

  if (is.null(parser)) {
    # stop("Need to pass parser function to read_eye")
    message("Only generic read and validation applied")
  } else{
    message("eye message parsing applied via: ", yaml)
    eye <- parser(eye, ...)
  }

  ### 4. Split pupil into separate structure


  ### 5. Gaze QA


  ### 6. Pupil QA


  ### 7. Diagnostic Plots



  #other general post-processing for eye data
  return(eye)
}



