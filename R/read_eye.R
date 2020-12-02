#' general wrapper for reading eye data into the package and performing all QA and processing
read_process_eye <- function(file, config = NULL, gen_log = TRUE, log_dir = NULL, ...) {
  ######################## setup
  tic("total time") #for internal speed checks
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
  tic("1. read time")
  eye <- eye_orig <- read_edf(file, keep_asc=FALSE, parse_all=TRUE, samples = TRUE)[[1]]#, c. = stepC)[[1]]; inc(stepC) # samples=FALSE removes read-in of raw data (significantly speeding up computation speed), but we'll keep this for now.
  toc()

  ######
  ### 2. Perform basic initial validation checks and compute new variables
  ######
  tic("2. init time")
  eye <- eye_init <- initialize_eye(eye_orig)#, c. = stepC); inc(stepC)
  toc()

  #########
  ### 3. Parse eye messages from experiment.pipeline config file, specified in task.yaml.
  #########
  if (is.null(config)) {
    cat("Only generic read/validation of ep.eye object applied. No user-specified message parsing.")
  } else{
    tic("3. parse time")
    eye <- eye_parsed <- parse_config_eye(eye_init, config)#; inc(stepC)
    toc()
  }

  ######
  ### 4 "Tidy timeseries". Incl downsampling raw and pupil data, making sure trial, block_trial tags, etc look right and appends indicator columns from specified messages (e.g. start_recording/ stimulus_on).
  ######

  tic("4. timeseries time")
  eye <- eye_cleantime <- tidy_eye_timeseries(eye, config, use_raw = FALSE)#; inc(stepC)
  toc()

  #########
  ### 5. Tag data with AOI information.
  #########
  if (!"aoi" %in% names(config$definitions$eye)) {
    cat("No AOI configurations supplied. Skipping")
  } else{
    tic("5. aoi time")
    eye <- eye_aoi <- add_aois(eye, config, use_raw = FALSE)#; inc(stepC)
    toc()
  }
  toc(); beepr::beep()

  #########
  ### 5. Gaze QA
  #########




  #########
  ### 6. Pupil QA
  #########

  #########
  ### 7. Diagnostic Plots
  #########

  return(eye)
}



