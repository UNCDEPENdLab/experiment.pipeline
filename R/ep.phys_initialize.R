#' @title Initialize an ep.phys object
#' @description Setting up ep.phys data object and performing basic checks on the physio data being agnostic to task specifics
#'
#' @param splice_data physio data will need to be spliced to extract the task relevant data
#' 
#' @author Nidhi Desai

# ######
## 2. Load acq data and perform basic initial validation checks
# - read_acq - read acq file into R
# - Initial validation 
######

# NOTE: DO NOT pass the config file into this function. Have the value needed as an input variable that can 
# be extracted from the config file while calling this ep.phys_initialize.R function.

ep.phys_initialize <- function(file,
                               task = NULL,
                               config_ecg,
                               config_eda,
                               ttl_codes_task = NULL, # TODO this needs the value of splice_data from initialization
                               meta_check = NULL
                               ){
  ### 2.1 Read ACQ file
  phys_data <- read_acq(file,
                        hdf5_output_dir = "~/temp_acq", # TODO generalize paths later
                        acq2hdf5_location = "~/Library/Python/3.7/bin/acq2hdf5")
  
  ### 2.2 initialize basic physio object structure
  ep.physio <- ep.phys_setup_data_structure(phys_data, 
                                            config_ecg = config_ecg,
                                            config_eda = config_eda,
                                            task = task)
  
  ### 2.3. Recode TTL code vector to reflect changes in the value, compute onsets and offsets, add labels to the codes
  ep.physio <- augment_ttl_details(ep.physio,
                                   zero_code = ttl_codes_task$zero_code,
                                   code_labels_df = ttl_codes_task$info) # TODO need to figure how to input this from config file config$proc_config$code_labels_df
  
  ### 2.3 Splice an ep.physio object to only the current task
  if (ttl_codes_task$splice_data == 1){ # do we need to splice the data for the specific task incase of physio data collected for longer time than the task itself?
    ep.physio <- splice_physio(ep.physio,
                               start_code = ttl_codes_task$start_task_code,
                               end_code = ttl_codes_task$end_task_code,
                               other_codes = ttl_codes_task$all_mid_codes)
    # TODO in the ep.phys_build_ttl_seq() function, add mid_ttl_codes to ttl_codes_task$all_mid_codes
  }

  ### 2.4 calculate task recording length
  ep.physio <- ep.phys_get_recording_length(ep.physio)
  
  ### Calculate other things??
  
  
  ### 2.5 Check metadata
  dt <- "- 2.5 Check ep.physio metadata:"
  if(!is.null(meta_check)){
    ep.physio <- ep.phys_meta_checks(ep.physio,
                                meta_vars = meta_check$meta_vars,
                                meta_vals = meta_check$meta_vals,
                                recording_time = meta_check$recording_time)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }
  
  ### 2.6 
  ep.physio <- ep.phys_validate_ttl_codes(ep.physio, 
                                          ttl_codes_freq = ttl_codes_task$expected_freq)



}



