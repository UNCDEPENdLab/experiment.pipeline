#' general wrapper for reading eye data into the package and performing all QA and processing
read_process_eye <- function(file, config = NULL, gen_log = TRUE, log_dir = NULL, ...) {
  ######################## setup
  # tic("total time") #for internal speed checks
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
  eye <- eye_init <- initialize_eye(eye_orig, config)#, c. = stepC); inc(stepC)
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

  #########
  ### 4. Gaze preprocessing
  #########
  tic("4. gaze preproc time")
  eye <- eye_gazePre <- preprocess_gaze(eye, config)
  toc()

  ######
  ### 5 Pupil preprocessing.
  ######
  tic("5. pupil time")
  eye <- pupil_preproc <- preprocess_pupil(eye, config)
  toc()

  ######
  ### 6. Remove raw data to cut the size of returned object considerably.
  ######
  if(!config$definitions$eye$return_raw){
    log_chunk_header("6. Remove raw data")
    eye$raw <- NULL
  }

  #close .elog
  sink()

  # to signal that preprocessing finished without issue (though make sure to check .elog for missteps along the way)
  class(eye) <- c(class(eye), "ep.eye.preproc")

  return(eye)
}



