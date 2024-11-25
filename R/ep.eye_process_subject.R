#' @title Read and process a single .edf file.
#' @description This is the main worker function that processes a single subject through the ep.eye preprocessing pipeline. It takes as arguments the paths to an .edf file and corresponding configuration YAML file and exports a preprocessed ep.eye object for a single subject.
#' @param file Path to the .edf file to process.
#' @param config_path Path to corresponding .yml configuration file with processing instructions. Instructions on how to effectively set up a configuration file can be found [HERE].
#' @param step complete preprocessing up to which step?
#' @param ... Optional arguments to pass to \code{read_edf.R} function.
#' @return A fully processed ep.eye object. [DETAILS HERE]
#' @details This function is ideally used within the \code{ep_batch_process_eye.R}
#' @details
#' data.table notes: one annoying challenge is making sure that data.table has openMP support enabled. See https://github.com/Rdatatable/data.table/issues/5419#issuecomment-1465023581 and https://stackoverflow.com/questions/76374014/use-openmp-on-m2-mac-with-r-and-data-table. You will need to download a few packages and update Makevars and your bashrc file.
#'
#' @examples
#'  \dontrun{
#'    file <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data/Neighborhood_PSU/Eye/004_AZ_Neighborhood_Eye.edf"
#'    config_path <- "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Neighborhood_PSU.yaml"
#'    ep.eye <- read_process_eye(file, config_path)
#'  }
#' @author Nate Hall
#'
#' @importFrom tictoc tic toc
#' @importFrom readr parse_number
#'
#' @export

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
  setwd("~/r_packages/experiment.pipeline")
  edf_raw <- "~/Documents/github_repos/arl_repos/dimt_analysis/data_raw/eye/dimt/92.edf"
  config_path <- "~/Documents/github_repos/arl_repos/dimt_analysis/R/psych.pipeline/config/dimt_eye_config.yaml"
  step <- NULL
  devtools::load_all()
  # -------------------------

  capture.output(pacman::p_load(tictoc,
                                readr,
                                tidyverse,
                                data.table,
                                yaml,
                                checkmate,
                                tryCatchLog,
                                nate.utils,
                                futile.logger,
                                dtplyr,
                                bannerCommenter)) -> tmp; rm(tmp)


  ######
  ### 1. Setup processing configuration variables and build block and event-specific message sequences.
  ######
  if (is.null(step) || step == "config") {
    tic("1. setup config time")
    config <- ep.eye_setup_proc_config(edf_raw,
                                       config_path,
                                       header = c("1. Setup ep.eye Processing [config] Options:",
                                                  "--------------------------------------------",
                                                  "- Review this output carefully!",
                                                  "- Every ep.eye processing function takes config as an input.",
                                                  "- Thus, config controls the behavior of every part of data processing moving forward!"))
    # do some session configuring
    setwd(config$definitions$eye$global$base_dir)

    ## setting up futile.logger options for logging files, but only do this if requested (default = FALSE)
    if(is.list(config$definitions$eye$global$log)){
      options(keep.source = TRUE)        # source code file name and line number tracking

      options("tryCatchLog.write.error.dump.file" = TRUE) # dump for post-mortem analysis
      options("tryCatchLog.write.error.dump.folder" = config$definitions$eye$global$log$error_dump)

      log_path <- file.path(config$definitions$eye$global$log$log_dir, paste0(config$definitions$eye$global$id, ".elog"))
      if (file.exists(log_path) & !config$definitions$eye$global$log$append) {
        file.remove(log_path)
        flog.appender(appender.file(log_path))  # to log into a file instead of console
      } else{
        # keep appending to what exists
        flog.appender(appender.file(log_path))  # to log into a file instead of console
      }

      flog.threshold(INFO)    # TRACE, DEBUG, INFO, WARN, ERROR, FATAL
    }

    toc()
    if (!is.null(step)) return(eye_raw)
  }

  if (is.null(step) || step == "init") {
    tic("2. init time")
    eye_init <- ep.eye_initialize(edf_raw,
                                  config,
                                  expected_edf_fields = config$definitions$eye$initialize$expected_edf_fields,
                                  task = config$definitions$eye$global$task,
                                  id = config$definitions$eye$global$id,
                                  gaze_events = config$definitions$eye$initialize$unify_gaze_events$gaze_events,
                                  # confirm_correspondence = config$definitions$eye$initialize$unify_gaze_events$confirm_correspondence,
                                  meta_check = config$definitions$eye$initialize$meta_check,
                                  inherit_btw_ev = config$definitions$eye$initialize$inherit_btw_ev,
                                  header = c("2. Initialize ep.eye object:",
                                             "-----------------------------",
                                             "- Checks for expected edf fields (raw, sacc, fix, blinks, etc) and gets your ep.eye object setup",
                                             "- This function also does some simple tidying/processing such as calculating the session recording length, checking",
                                             "  that timestamps in the raw data do not have big gaps, and unifies raw and gaze event data by merging raw data with gaze events by",
                                             "  generating unique gaze event numbers, merges them to raw data and validates that timestamps from raw and gaze event fields are in correspondence.",
                                             "- From here, if there are messages passed to the eyetracker between recording events, pull these into the gaze data for this subject (see",
                                             "  ep.eye_store_between_event_messages)",
                                             "- Unifies messages into the raw data by merging message timestamps into the raw data",
                                             "- Performs a check of the metadata (see config documentation)",
                                             "- Shifts timestamps in raw and gaze events data to a zero start point"
                                  )
    )
    toc()
    if (!is.null(step)) return(eye_init)
  }

  if (is.null(step) || step == "parse") {
    tic("3. parse time")
    eye_parsed <- ep.eye_parse_events(eye_init,
                                      config,
                                      extract_event_func_path = config$definitions$eye$msg_parse$extract_event_func_path,
                                      csv_path = file.path(config$definitions$eye$msg_parse$csv_dir_path, paste0(config$definitions$eye$global$id, ".csv")),
                                      msg_seq = config$definitions$eye$msg_parse$msg_seq,
                                      header = "3. Parse task events:")
    toc()
    if (!is.null(step)) return(eye_parsed)
  }

  if (is.null(step) || step == "gaze_preproc") {
    tic("4. gaze preproc time")
    eye_gazePre <- ep.eye_preprocess_gaze(eye_parsed,
                                          config,
                                          aoi = config$definitions$eye$gaze_preproc$aoi,
                                          downsample = config$definitions$eye$gaze_preproc$downsample,
                                          header = "4. Gaze preprocessing:")
    toc()
    if (!is.null(step)) return(eye_gazePre)
  }

  # if (is.null(step) || step == "pupil_preproc") {
  #   tic("5. pupil time")
  #   eye_gaze_pupilPre <- ep.eye_preprocess_pupil(eye_gazePre,
  #                                                config,
  #                                                blink_corr = config$definitions$eye$pupil_preproc$blink_corr,
  #                                                filter = config$definitions$eye$pupil_preproc$filter,
  #                                                interpolate = config$definitions$eye$pupil_preproc$interpolate,
  #                                                baseline_correction = config$definitions$eye$pupil_preproc$baseline_correction,
  #                                                downsample = config$definitions$eye$pupil_preproc$downsample,
  #                                                header = "5. Pupil preprocessing:")
  #   toc()
  #   if (!is.null(step)) return(eye_gaze_pupilPre)
  # }

  if (is.null(step) || step == "cleanup") {
    tic("6. cleanup time")
    ep.eye_clean <- ep.eye_cleanup(eye_gazePre,
                                   config,
                                   globals = config$definitions$eye$global,
                                   header = "6. Cleanup and export ep.eye: ")
    toc()
    return(ep.eye_clean)
  }
}




