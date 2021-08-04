#' @title Read and process a single .edf file.
#' @description This is the main worker function that processes a single subject through the ep.eye preprocessing pipeline. It takes as arguments the paths to an .edf file and corresponding configuration YAML file and exports a preprocessed ep.eye object for a single subject.
#' @param file Path to the .edf file to process.
#' @param config_path Path to corresponding .yml configuration file with processing instructions. Instructions on how to effectively set up a configuration file can be found [HERE]. 
#' @param ... Optional arguments to pass to \code{read_edf.R} function.
#' @return A fully processed ep.eye object. [DETAILS HERE]
#' @details This function is ideally used within the \code{ep_batch_process_eye.R}
#' @examples
#'  \dontrun{
#'    file <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data/Neighborhood_PSU/Eye/004_AZ_Neighborhood_Eye.edf"
#'    config_path <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Neighborhood_PSU.yaml"
#'    ep.eye <- read_process_eye(file, config_path)
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
  # file <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data/Neighborhood_PSU/Eye/004_AZ_Neighborhood_Eye.edf"
  # config_path <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Neighborhood_PSU.yaml"
  # # Sorting Mushrooms 
  #  file <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data/SortingMushrooms_PSU/Eye/010_AE_SortingMushrooms_Eye.edf"
  #  config_path <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Sorting_Mushrooms.yaml"
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
  ### 2. Perform basic initial validation checks and compute new variables
  ######
  tic("2. init time")
  eye_init <- ep.eye_initialize(file, 
                                expected_edf_fields = config$definitions$eye$initialize$expected_edf_fields,
                                task = config$task,
                                gaze_events = config$definitions$eye$initialize$unify_gaze_events,
                                meta_check = config$definitions$eye$initialize$meta_check,
                                inherit_btw_ev = config$definitions$eye$initialize$inherit_btw_ev,
                                header = "2. Initialize ep.eye object:")
  toc()

  #########
  ### 3. Parse eye messages from experiment.pipeline config file, specified in task yaml.
  #########
  if (is.null(config$definitions$eye$msg_parse)) {
    cat("3. Parse task events: SKIP (Only generic read/validation of ep.eye object applied. No user-specified message parsing)")
  } else{
    tic("3. parse time")
    eye_parsed <- ep.eye_parse_events(eye_init, 
                                      extract_event_func_path = config$definitions$eye$msg_parse$extract_event_func_path,
                                      csv_path = file.path(config$definitions$eye$msg_parse$csv_dir_path, paste0(config$definitions$eye$global$prefix, ".csv")),
                                      msg_seq = config$definitions$eye$msg_parse$msg_seq,
                                      header = "3. Parse task events:")
    toc()
  }

  #########
  ### 4. Gaze preprocessing
  #########
  tic("4. gaze preproc time")
  eye_gazePre <- ep.eye_preprocess_gaze(eye_parsed, 
                                        aoi = config$definitions$eye$gaze_preproc$aoi,
                                        downsample = config$definitions$eye$gaze_preproc$downsample,
                                        header = "4. Preprocess gaze data:")
  toc()

  ######
  ### 5 Pupil preprocessing.
  ######
  tic("5. pupil time")
  eye_gaze_pupilPre <- ep.eye_preprocess_pupil(eye_gazePre, 
                                        blink_corr = config$definitions$eye$pupil_preproc$blink_corr,
                                        filter = config$definitions$eye$pupil_preproc$filter,
                                        interpolate = config$definitions$eye$pupil_preproc$interpolate,
                                        baseline_correction = config$definitions$eye$pupil_preproc$baseline_correction,
                                        downsample = config$definitions$eye$pupil_preproc$downsample,
                                        header = "5. Pupil preprocessing:")
  toc()

  ######
  ### 6 Cleanup.
  ######
  tic("6. cleanup time")
  ep.eye_clean <- ep.eye_cleanup(eye_gaze_pupilPre,
                                 globals = config$definitions$eye$global,
                                 header = "6. Cleanup and export ep.eye: ")
  toc()

  return(ep.eye_clean)
}