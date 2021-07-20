#' @title Read and process a single .edf file.
#' @description This is the main worker function that processes a single subject through the ep.eye preprocessing pipeline. It takes as arguments the paths to an .edf file and corresponding configuration YAML file and exports a preprocessed ep.eye object for a single subject.
#' @param file Path to the .edf file to process.
#' @param config_path Path to corresponding .yml configuration file with processing instructions. Instructions on how to effectively set up a configuration file can be found [HERE]. 
#' @param ... Optional arguments to pass to \code{read_edf.R} function.
#' @return A fully processed ep.eye object. [DETAILS HERE]
#' @details This function is ideally used within the \code{ep_batch_process_eye.R}
#' @examples
#'  \dontrun{
#'    ep.eye <- read_process_eye("/proj/mnhallqlab/studies/NeuroMAP/s3_data/Neighborhood_PSU/eye/002_HS_Neighborhood_Eye.edf", "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Neighborhood_PSU.yaml")
#'  }
#' @author Nate Hall
#' 
#' @importFrom tictoc tic toc
#' 
#' @export
read_process_eye <- function(file, config_path, ...) {# = NULL, prefix = NULL, gen_log = TRUE, log_dir = NULL, save_preproc = FALSE, out_dir = NULL, event_csv = NULL, ...) { # 6/7: move all of these options into the config file for one-stop shopping. 
  ######################### load files for debugging. comment when running full.
 source("/proj/mnhallqlab/users/nate/experiment.pipeline/NH_local/setup_envi.R") ## once package and dependencies are installed and load properly, this will be accomplished by loading the package library.
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
  eye <- eye_evtag <- tag_event_time(eye); cat("- 6.1 time_ev column generated: COMPLETE\n")


  # to signal that preprocessing finished without issue (though make sure to check .elog for missteps along the way)
  class(eye) <- c(class(eye), "ep.eye.preproc")

  ######
  ### 7. Remove raw data to cut the size of returned object considerably.
  ######
  if(!config$definitions$eye$process_opts$return_raw){
    log_chunk_header("7. Removing raw data")
    eye$raw <- NULL
    cat("- 7.1 Removing raw data: COMPLETE\n")
  } else{
    log_chunk_header("7. Removing raw data: SKIPPED")
  }

  ######
  ### 8. Save preprocessed data
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



