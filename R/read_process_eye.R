#' general wrapper for reading eye data into the package and performing all QA and processing
read_process_eye <- function(file, config_path) {# = NULL, prefix = NULL, gen_log = TRUE, log_dir = NULL, save_preproc = FALSE, out_dir = NULL, event_csv = NULL, ...) { # 6/7: move all of these options into the config file for one-stop shopping. 
  ######################### load files for debugging. comment when running full.
 source("/proj/mnhallqlab/users/nate/experiment.pipeline/NH_local/setup_envi.R")
 # Neighborhood - PSU
 file <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data/Neighborhood_PSU/eye/002_HS_Neighborhood_Eye.edf"
 config_path <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Neighborhood_PSU.yaml"
 # Neighborhood - UNC

 ######################## setup
  config <- setup_proc_configs(file, config_path)

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
    eye <- eye_parsed <- parse_config_eye(eye_init, config, event_csv = event_csv)#; inc(stepC)
    toc()
  }

  #########
  ### 4. Gaze preprocessing
  #########
  tic("4. gaze preproc time")
  eye <- eye_gazePre <- preprocess_gaze(eye_parsed, config)
  toc()

  ######
  ### 5 Pupil preprocessing.
  ######
  tic("5. pupil time")
  eye <- eye_gaze_pupilPre <- preprocess_pupil(eye_gazePre, config)
  toc()


  ######
  ### 6 Generate timing by event and trial.
  ######
  log_chunk_header("6. Generating event-locked timings")
  eye <- eye_evtag<- tag_event_time(eye); cat("- 6.1 time_ev column generated: COMPLETE\n")


  # to signal that preprocessing finished without issue (though make sure to check .elog for missteps along the way)
  class(eye) <- c(class(eye), "ep.eye.preproc")

  ######
  ### 7. Remove raw data to cut the size of returned object considerably.
  ######
  if(!config$definitions$eye$return_raw){
    log_chunk_header("7. Removing raw data")
    eye$raw <- NULL
    cat("- 7.1 Removing raw data: COMPLETE\n")
  } else{
    log_chunk_header("7. Removing raw data: SKIPPED")
  }

  ######
  ### 8. Saving preprocessed data
  ######
  if(save_preproc){
    log_chunk_header("8. Saving preprocessed data")
    if(is.null(out_dir)) {
      spath <- paste0(prefix, "_ep.eye.preproc.RData")
    } else{
      if(!dir.exists(out_dir)) dir.create(out_dir)
      spath <- file.path(out_dir, paste0(prefix, "_ep.eye.preproc.RData"))
    }
    save(eye, file = spath); cat(paste0("- 8.1 Preprocessed data saved to ", spath, ": COMPLETE\n"))
  }

  #close .elog
  sink(); sink()

  return(eye)
}



