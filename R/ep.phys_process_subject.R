# testing
pacman::p_load(checkmate, data.table, tidyverse, yaml, rhdf5, RHRV, experiment.pipeline)
acq_filepath <- "/proj/mnhallqlab/studies/neuromap/data/physio_s3/raw_acq/713_gw_physio.acq"
config_path <- "/proj/mnhallqlab/users/nidhi/exp.pipe_physio/experiment.pipeline/inst/extdata/ep_configs/kingdom/kingdom_phys.yaml"
sapply(list.files(path = "/proj/mnhallqlab/users/nidhi/exp.pipe_physio/experiment.pipeline/R", full.names = TRUE), source)


#' @title Read and process a single .acq file.
#' @description This is the main worker function that processes a single subject through the ep.phys preprocessing pipeline. It takes as arguments the paths to an .acq file and corresponding configuration YAML file and exports a preprocessed ep.phys object for a single subject.
#' @author Nidhi Desai
ep.phys_process_subject <- function(acq_filepath, config_path, multiple_subjects = FALSE, ...) {

  ######
  ## 1. Validate config file
      # - import the yaml config file
      # - check if all required data exist in the config file
      # - file in the default values for the variables that don't exist
      # - process the config[["blocks"]] section to create config$proc_config$code_labels_df
          # - Also add config$proc_config$all_mid_codes
  ######
  config <- ep.phys_setup_proc_config(acq_filepath, config_path)
  phys_config <- config$definitions$physio

  ######
  ## 2. Load acq data and perform basic initial validation checks
      # - read_acq - read acq file into R
      # - Initial validation 
  ## what creates the ep.physio structure? - read_acq function
  ######

  phys_init <- ep.phys_initialize(acq_filepath,
                                  task,
                                  config_ecg = phys_config$ecg_preproc,
                                  config_eda = phys_config$eda_preproc,
                                  ttl_code_task = config$definitions$physio$initialize$ttl_code_task,
                                  meta_check = config$definitions$physio$initialize$meta_check)

  ######
  ## 3. ECG processing and QA
  ######
  phys_ecg <- ep.phys_preprocess_ecg(ep.physio,
                                     detect_beats = !is.null(config$definitions$physio$ecg_preproc$beta_detection),
                                     wfdb_path = config$definitions$physio$ecg_preproc$beta_detection$wfdb_path,
                                     detectors = config$definitions$physio$ecg_preproc$beta_detection$beats_detector,
                                     ibi_qa = !is.null(config$definitions$qa$ecg$ibi),
                                     ibi_outliers = config$definitions$qa$ecg$ibi$ibi_outlier)

  

  ######
  ## 4. EDA processing and QA
  ######

  ep.physio <- ep.phys_preprocess_eda(ep.physio, phys_config)



}






