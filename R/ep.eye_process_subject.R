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
ep.eye_process_subject <- function(file, config_path, ...) {
 
 ######################### load example files for debugging. comment when running full.
 source("/proj/mnhallqlab/users/nate/experiment.pipeline/NH_local/setup_envi.R") ## once package and dependencies are installed and load properly, this will be accomplished by loading the package library.
#  # Neighborhood - PSU
#  file <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data/Neighborhood_PSU/Eye/004_AZ_Neighborhood_Eye.edf"
#  config_path <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Neighborhood_PSU.yaml"
 # Sorting Mushrooms 
 file <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data/SortingMushrooms_PSU/Eye/010_AE_SortingMushrooms_Eye.edf"
 config_path <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Sorting_Mushrooms.yaml"
 # Neighborhood - UNC

 ######################## 
 
  ######
  ### 1. Setup processing configuration variables and build block and event-specific message sequences.
  ######
  tic("1. setup config time")
  config <- ep.eye_setup_proc_config(file, 
                                     config_path,
                                     header = "1. Setup Processing Options:") 
  toc()

  ######
  ### 2. Read EDF file
  ######
  tic("2. read time")
  eye <- eye_orig <- read_edf(file, 
                              keep_asc=FALSE, 
                              parse_all=TRUE, 
                              samples = TRUE, 
                              header = "2. Read EDF file:")[[1]]
  toc()

  ######
  ### 3. Perform basic initial validation checks and compute new variables
  ######
  tic("3. init time")
  eye_init <- ep.eye_initialize(eye_orig, 
                                expected_edf_fields = config$definitions$eye$initialize$expected_edf_fields,
                                task = config$task,
                                gaze_events = config$definitions$eye$initialize$unify_gaze_events,
                                meta_check = config$definitions$eye$initialize$meta_check,
                                inherit_btw_ev = config$definitions$eye$intialize$inherit_btw_ev,
                                header = "3. Initialize eye object:")
  toc()

  #########
  ### 4. Parse eye messages from experiment.pipeline config file, specified in task yaml.
  #########
  if (is.null(config$definitions$eye$msg_parse)) {
    cat("4. Parse task events: SKIP (Only generic read/validation of ep.eye object applied. No user-specified message parsing)")
  } else{
    tic("3. parse time")
    eye_parsed <- ep.eye_parse_events(eye_init, 
                                      extraction_method = config$definitions$eye$msg_parse$extraction_method,
                                      extract_event_func_path = config$definitions$eye$msg_parse$extract_event_func_path,
                                      csv_path = file.path(config$definitions$eye$msg_parse$csv_dir_path, paste0(config$definitions$eye$global$prefix, ".csv")),
                                      msg_seq = config$definitions$eye$msg_parse$msg_seq,
                                      header = "4. Parse task events:")
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
  if(return_raw){
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



