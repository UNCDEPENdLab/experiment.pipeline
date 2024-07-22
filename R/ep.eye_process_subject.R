
ep.eye_process_subject <- function(edf_raw,
                                   config_path,
                                   step = NULL,
                                   ...) {
  # debug:
  # -------------------------
  # source("/proj/mnhallqlab/users/nate/experiment.pipeline/NH_local/setup_envi.R") ## once package and dependencies are installed and load properly, this will be accomplished by loading the package library.
  #  # Neighborhood - PSU
  # ---------------------
  # edf_raw <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data/Neighborhood_PSU/Eye/004_AZ_Neighborhood_Eye.edf"
  # config_path <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Neighborhood_PSU.yaml"
  # # Sorting Mushrooms
  # -------------------
  #  edf_raw <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data/SortingMushrooms_PSU/Eye/010_AE_SortingMushrooms_Eye.edf"
  #  config_path <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Sorting_Mushrooms.yaml"
  # Neighborhood - UNC

  #inst files come with package
  # ---------------------------
  # edf_files <- list.files(file.path(rprojroot::find_package_root_file(), "inst/extdata/raw_data/Neighborhood/Eye"), full.names = TRUE)
  # edf_files <- list.files(file.path(rprojroot::find_package_root_file(), "inst/extdata/raw_data/SortingMushrooms/Eye"), full.names = TRUE)
  # edf_raw <- edf_files[3] # extract a single subject for example case
  # config_path <- file.path(rprojroot::find_package_root_file(), "inst/extdata/ep_configs/Neighborhood/Neighborhood.yaml")
  # config_path <- file.path(rprojroot::find_package_root_file(), "inst/extdata/ep_configs/SortingMushrooms/SortingMushrooms.yaml")

  # Dimensions + Threat
  # -------------------
  # edf_raw <- "~/Documents/github_repos/arl_repos/dimt_analysis/data_raw/eye/dimt/595.edf"
  # config_path <- "~/Documents/github_repos/arl_repos/dimt_analysis/config/dimt_eye_config.yaml"
  # step <- NULL
  # devtools::load_all()
  # -------------------------

  library(tictoc)
  library(readr)
  library(tidyverse)
  library(data.table)
  library(yaml)
  library(checkmate)
  library(tryCatchLog)
  library(nate.utils)
  library(futile.logger)

  ######
  ### 1. Setup processing configuration variables and build block and event-specific message sequences.
  ######
  if (is.null(step) || step == "config") {
    tic("1. setup config time")
    config <- ep.eye_setup_proc_config(edf_raw,
                                       config_path,
                                       header = c("1. Setup Processing Options:",
                                                  "-----------------------------",
                                                  "- Review this output carefully!",
                                                  "- Every ep.eye processing function takes config as an input.",
                                                  "- Thus, config controls the behavior of every part of data processing moving forward!"))
    # ep.outpath <- file.path(config$definitions$eye$global$base_dir, config$definitions$eye$global$log$log_dir, "config", config$id)
    #
    # saveRDS()
    toc()
    if (!is.null(step)) return(eye_raw)
  }

  if (is.null(step) || step == "init") {
    tic("2. init time")
    eye_init <- ep.eye_initialize(eye_raw, config)
    toc()
    if (!is.null(step)) return(eye_init)
  }

  if (is.null(step) || step == "parse") {
    tic("3. parse time")
    eye_parsed <- ep.eye_parse_events(eye_init,
                                      extract_event_func_path = config$definitions$eye$msg_parse$extract_event_func_path,
                                      csv_path = file.path(config$definitions$eye$msg_parse$csv_dir_path, paste0(config$definitions$eye$global$prefix, ".csv")),
                                      msg_seq = config$definitions$eye$msg_parse$msg_seq,
                                      header = "3. Parse task events:")
    toc()
    if (!is.null(step)) return(eye_parsed)
  }

  if (is.null(step) || step == "gaze_preproc") {
    tic("4. gaze preproc time")
    eye_gazePre <- ep.eye_preprocess_gaze(eye_parsed,
                                          aoi = config$definitions$eye$gaze_preproc$aoi,
                                          downsample = config$definitions$eye$gaze_preproc$downsample,
                                          header = "4. Gaze preprocessing:")
    toc()
    if (!is.null(step)) return(eye_gazePre)
  }

  if (is.null(step) || step == "pupil_preproc") {
    tic("5. pupil time")
    eye_gaze_pupilPre <- ep.eye_preprocess_pupil(eye_gazePre,
                                                 blink_corr = config$definitions$eye$pupil_preproc$blink_corr,
                                                 filter = config$definitions$eye$pupil_preproc$filter,
                                                 interpolate = config$definitions$eye$pupil_preproc$interpolate,
                                                 baseline_correction = config$definitions$eye$pupil_preproc$baseline_correction,
                                                 downsample = config$definitions$eye$pupil_preproc$downsample,
                                                 header = "5. Pupil preprocessing:")
    toc()
    if (!is.null(step)) return(eye_gaze_pupilPre)
  }

  if (is.null(step) || step == "cleanup") {
    tic("6. cleanup time")
    ep.eye_clean <- ep.eye_cleanup(eye_gaze_pupilPre,
                                   globals = config$definitions$eye$global,
                                   header = "6. Cleanup and export ep.eye: ")
    toc()
    return(ep.eye_clean)
  }
}



#
#
# # edf_raw <- "~/Documents/github_repos/arl_repos/dimt_analysis/data_raw/eye/dimt/595.edf"
# config_path <- "~/Documents/github_repos/arl_repos/dimt_analysis/config/dimt_eye_config.yaml"
#
# x <- ep.eye_process_subject(edf_raw, config_path = config_path, step = "config")
