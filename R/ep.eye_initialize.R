#' generic function for initializing ep.eye object and performing basic internal checks on the eye data, while remaining agnostic to task/behavior structure.
#'
#' This includes validation of very basic data quality (large variance in gaze distribution, excessive blinks, large jumps in eye position, etc).
#' TODO: include functionality for logging of successes, warnings, failures. This will probably involve a trycatch statement that could handle a potentially large number of issues. We'll have to see how complicated it gets by balancing flexibility with parsimony. Tend to prefer flexibility if the package allows user-side functionality to be parsimonious :)
#' TODO: perhaps even store key variables (e.g. some measure of pupil fluctuation, or saccade velocity/acceleration) from prior subjects in separate circumscribed csv (which values get appended to) and plot distributions for every new subject. This would be akin to constructing a sort of empirical null distribution and performing informal (visual)"hypothesis tests" where we would hope certain variables in a given subject are not "significantly different" than the group distribution.
#' @param eye raw eye object pulled directly from the .edf file using read_edf(). Must be a list with expected_edf_fields c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file").
#' @importFrom lubridate seconds_to_period


ep.eye_initialize <- function(file, 
                              expected_edf_fields = c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file"), 
                              task = NULL,
                              gaze_events = c("sacc", "fix", "blink"),
                              meta_check = NULL, 
                              inherit_btw_ev = NULL,
                              header = "2. Initialize eye object:",
                              ...){

  ## debug
  expected_edf_fields = config$definitions$eye$initialize$expected_edf_fields
  task = config$task
  gaze_events = config$definitions$eye$initialize$unify_gaze_events
  meta_check = config$definitions$eye$initialize$meta_check
  inherit_btw_ev = config$definitions$eye$initialize$inherit_btw_ev
  header = "2. Initialize eye object:"

  log_chunk_header(header)

  ### 2.1 Read EDF file
  tryCatch.ep({
    eye <- read_edf(file, keep_asc=FALSE, parse_all=TRUE, samples = TRUE)[[1]]
  }, describe_text = "- 2.1 Read EDF file:")

  ### 2.2 make sure all names are present
  tryCatch.ep({
    stopifnot(all(expected_edf_fields %in% names(eye)))
  }, describe_text = "- 2.2 Check expected fields:")

  ### 2.3 initialize basic eye object structure
  tryCatch.ep({
    ep.eye <- ep.eye_setup_structure(eye, task = task)
  }, describe_text = "- 2.3 Initialize ep.eye list structure:")

  ### 2.4 document entire recording session length (if this is very different from BAU this should get flagged later)
  tryCatch.ep({
    ep.eye <- ep.eye_get_session_length(ep.eye)
  }, describe_text = paste0("- 2.4 Document recording session length (", seconds_to_period(round(ep.eye$metadata$recording_time)),"):", sep = ""))

  ### 2.5 check for continuity in timestamp on raw data
  tryCatch.ep({
    ep.eye <- ep.eye_raw_sample_continuity_check(ep.eye)
  }, describe_text = "- 2.5 Check for continuity in raw data:")

  ### 2.6 Unify gaze events.
  if(!is.null(gaze_events)){
    cat(paste0("- 2.6 Unify gaze events(", paste0(gaze_events, collapse = ", "), ") and raw data:\n"))
    ep.eye <- ep.eye_backup <-  ep.eye_unify_gaze_events(ep.eye, gaze_events = gaze_events) # store backup for testing
  } else {
    "- 2.6 Unify gaze events: SKIP"
  }

  ### 2.7 Store between-event messages
  tryCatch.ep({
    ep.eye <- ep.eye_store_between_event_messages(ep.eye)
  }, "- 2.7 Store between-event messages:")

  ### 2.8 rm cr.info
  tryCatch.ep({
    ep.eye <- ep.eye_rm_crinfo(ep.eye)
  }, "- 2.8 Remove cr.info column from raw data:")

  ### 2.9 Unify et.msgs into raw data.
  tryCatch.ep({
    ep.eye <- ep.eye_unify_raw_msg(ep.eye)
  }, "- 2.9 Unify et.msgs into raw data:")

  ### 2.10 Check metadata
  dt <- "- 2.10 Check ep.eye metadata:"
  if("meta_check" %in% names(config$definitions$eye$initialize)){
    ep.eye <- ep.eye_meta_check(ep.eye, 
               meta_vars = config$definitions$eye$initialize$meta_check$meta_vars,
               meta_vals = config$definitions$eye$initialize$meta_check$meta_vals,
               recording_time = config$definitions$eye$initialize$meta_check$recording_time,
               dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 2.11 Shift timestamps to 0 start point
  ep.eye <- ep.eye_shift_timing(ep.eye,"- 2.11 Shift timestamps to 0 start point:")

  ### 2.12 Extract important between-event messages if requested. N.B. 4.1.2 move_to_within still needs work!!!
  dt <- "- 2.12 Inherit between-event messages, calibration checks:\n"
  if(!is.null(inherit_btw_ev)){
    ep.eye <-  ep.eye_inherit_btw_ev(ep.eye, 
                                     inherit_btw_ev, 
                                     dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }
  
  return(ep.eye)
}

