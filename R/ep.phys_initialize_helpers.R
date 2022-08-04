############################
##### List of subsidiary functions utilized in `ep.eye_parse_events()`
############################
# - ep.phys_setup_structure()
# - ep.phys_get_task_length()
# - ep.phys_meta_checks()
# - ep.phys_validate_ttl_codes()
# -- ep.phys_ttl_frequency_check()
# -- ep.phys_ttl_sequence_check()
############################


#' @title Initialize a ep.phys data structure
#' @description Initialize an empty ep.phys data structure which will get filled-in with actual data as the data passed through different functions of the pipeline. 
#' @param physio_data loaded .acq file from \code{read_acq}
#' @param config_ecg configuration for ecg data in list format
#' @param config_eda configuration for eda data in list format
#' @param task name of the task for which the data is processed
#' 
#' @importFrom data.table data.table
#' @return ep.physio returns a list of 5 objects of type lists or data.tables. These objects are raw data, ecg processed data and qa, 
#'   eda processed data and qa, ttl codes and metadata.
#' 
#' @author Nidhi Desai
#' 
#' @export
#' @examples TODO add examples
ep.phys_setup_structure <- function(physio_data, 
                                    config_ecg,
                                    config_eda,
                                    task = NULL){
  ep.physio <- list(raw = physio_data$raw,
                  ecg = list(processed = data.table(),
                             qa = list()),
                  eda = list(processed = data.table(),
                             qa = list()),
                  ttl_codes = data.table(),
                  metadata = list(sampling_rate = physio_data$sampling_rate,
                                  max_channel_rate = physio_data$max_channel_rate,
                                  acq_file = physio_data$acq_file,
                                  hdf5_file = physio_data$hdf5_file,
                                  ecg = list(config = config_ecg),
                                  eda = list(config = config_eda)) # TODO format in setup config function from input config file
  )
  # tag with ep.physio class
  class(ep.physio) <- c(class(ep.physio), "ep.physio")
  
  # rename EDA and ECG signal name in ep.physio$raw
  for (x in c("ecg", "eda")){
    raw_indx <- which(grepl(x, tolower(names(ep.physio$raw)), fixed = TRUE))
    if(length(raw_indx) == 0){
      ep.physio[[x]] <- NULL
      ep.physio[["metadata"]][[x]] <- NULL
    } else {
      # add the signal name to metadata
      ep.physio$metadata[[x]][["signalname"]] <- names(ep.physio$raw)[raw_indx]
      # rename the ecg signal in raw
      names(ep.physio$raw)[raw_indx] <- x
    }
  }

  # append task name to metadata
  if(!is.null(task)) ep.physio[["metadata"]][["task"]] <- task
  return(ep.physio)
}


#' @title Calculates the recording length
#' @description This function calculates the recording length of the session. If the raw data has been spliced for a task, then this will calculate the task length.
#' @param ep.physio An initialized ep.physio object. See \code{ep.phys_setup_structure} to initialize this object.
#' 
#' @return ep.physio object with recording_time added to \code{ep.physio$metadata}
#' 
#' @author Nidhi Desai
#' 
#' @export
ep.phys_get_recording_length <- function(ep.physio){
  # recording time for the task in seconds
  ep.physio$metadata$recording_time <- max(ep.physio$raw$time_s) - min(ep.physio$raw$time_s)
  return(ep.physio)
}


#' @title Checks on metadata 
#' @description This function perform checks on metadata like comparing actual values of session parameters (ex: sampling data) with expected values mentioned in config file. 
#'   Also checking if task recording time in physio data lies within the expectated range. Reference ep.eye_meta_check() functions for reference. # examples of metachecks: sample rate, max_channel_rate, recording length.
#' @param ep.physio An initialized ep.physio object. See \code{ep.phys_setup_structure} to initialize this object.
#' @param meta_vars Character vector of meta variables to check
#' @param meta_vals Character vector of values to expect for the meta variables passed above
#' @param recording_time Numeric vector of length 2 indicating the expected recording session time *in seconds* and the margin of error above and below the expected recording time without generating an error.
#' 
#' @return ep.physio object with \code{ep.physio$metadata$meta_check} added.
#' 
#' @author Nidhi Desai
#' 
#' @export
ep.phys_meta_checks <- function(ep.physio, 
                                meta_vars,
                                meta_vals,
                                recording_time){
  
  ### 2.5.1 meta_vars and vals
  stopifnot(!(is.null(meta_vars) | is.null(meta_vals))) # stop if either are null, need both.
  stopifnot(length(meta_vars) == length(meta_vals))
  
  meta_ref <- data.frame(meta_vars, meta_vals) %>% mutate_all(as.character)
  
  mismatch_var <- c() # append if any discrepancies.
  mismatch_expected <- c()
  mismatch_actual <- c()
  for(i in 1:nrow(meta_ref)){
    # message(eye$metadata[[meta_ref[i,"meta_vars"]]], " ", meta_ref[i,"meta_vals"])
    if(!ep.physio$metadata[[meta_ref[i,"meta_vars"]]] == meta_ref[i,"meta_vals"]){
      mismatch_var <- c(mismatch, meta_ref[i,"meta_vars"])
      mismatch_expected <- c(mismatch_expected, meta_ref[i,"meta_vals"])
      mismatch_actual <- c(mismatch_actual, ep.physio$metadata[[meta_ref[i,"meta_vars"]]])
    }
  }
  
  if(!is.null(mismatch_var)){
    warning("meta variables not match expected value: ", mismatch_var, call. = FALSE)
    ep.physio[["metadata"]][["meta_check"]][["meta_vars"]][["meta_vars_mismatch"]] <- mismatch_var
    ep.physio[["metadata"]][["meta_check"]][["meta_vars"]][["meta_vars_expected"]] <- mismatch_expected
    ep.physio[["metadata"]][["meta_check"]][["meta_vars"]][["meta_vars_actual"]] <- mismatch_actual
  } else {
    ep.physio[["metadata"]][["meta_check"]][["meta_vars"]] <- NULL
  }
  
  ### 2.5.2 confirm acceptable session length
  stopifnot(!is.null(recording_time))
  stopifnot(!is.null(ep.physio$metadata$recording_time))
  
  rt_range <- c(recording_time[1] - recording_time[2],
                recording_time[1] + recording_time[2])
  
  if(!all((rt_range[1] <= ep.physio$metadata$recording_time) & (ep.physio$metadata$recording_time <= rt_range[2]))){
    warning("recording length (", ep.physio$metadata$recording_time,") outside of expected bounds: ", paste0(rt_range, collapse = ", "), call. = FALSE)
    ep.physio[["metadata"]][["meta_check"]][["recording_length_violation"]] <- TRUE
    # TODO should we add the expected recording length here?
  } else{
    ep.physio[["metadata"]][["meta_check"]][["recording_length_violation"]] <- FALSE
  }
}



#' @title Perform validation checks on the parport codes frequency and sequence
#' @description 
ep.phys <- ep.phys_validate_ttl_codes(ep.physio){
  
  ep.phys <- ep.phys_ttl_frequency_check(ep.physio)
  
  
  ep.phys <- ep.phys_ttl_sequence_check(ep.physio)
  
}

#' @title check the frequency of parport codes
#' @description Check the actual frequency for each ttl_code in the output of augment_ttl_details() i.e. acq_data$ttl_codes, comparing it to the expected_freq in previous function's output. This function will latter be added to ep.phys_initialize_helpers() to perform data validations
ep.phys_ttl_frequency_check <- function(ep.physio){
  
}


#' @title check the sequence of parport codes
#' @description check if the ttl_codes are in the right sequence
ep.phys_ttl_sequence_check <- function(ep.physio){
  
}









