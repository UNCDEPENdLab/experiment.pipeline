#' general wrapper for reading eye data into the package and performing all QA and processing
read_process_eye <- function(file, config = NULL, gen_log = TRUE, log_dir = NULL, ...) {
  ######################## setup
  tictoc::tic() #for internal speed checks
  stopifnot(file.exists(file))
  if (length(file) > 1L) { stop("At present, read_process_eye is designed for one file at a time") }

  # initialize log file if requested. Otherwise will print feedback while running checks.
  ## N.B. right now this will overwrite existing files, can come back to later.
  if(gen_log){init_eyelog(log_dir)}

  ######################## Begin processing

  # # initialize step counter
  # stepC <- 1

  ######
  ### 1. Read EDF file
  ######
  eye <- eye_orig <- read_edf(file, keep_asc=FALSE, parse_all=TRUE, samples = TRUE)[[1]]#, c. = stepC)[[1]]; inc(stepC) # samples=FALSE removes read-in of raw data (significantly speeding up computation speed), but we'll keep this for now.

  ######
  ### 2. Perform basic initial validation checks and compute new variables
  ######
  eye <- eye_init <- initialize_eye(eye)#, c. = stepC); inc(stepC)
  tictoc::toc(); beepr::beep()

  #########
  ### 3. Parse eye messages from experiment.pipeline config file, specified in task.yaml.
  #########
  if (is.null(config)) {
    cat("Only generic read/validation of ep.eye object applied. No user-specified message parsing.")
  } else{
    eye <- eye_parsed <- parse_config_eye(eye, config)#; inc(stepC)
  }


  ### 4. Split pupil into separate structure


  ### 5. Gaze QA


  ### 6. Pupil QA


  ### 7. Diagnostic Plots



  #other general post-processing for eye data
  return(eye)
}



