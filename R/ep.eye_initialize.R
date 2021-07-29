#' generic function for initializing ep.eye object and performing basic internal checks on the eye data, while remaining agnostic to task/behavior structure.
#'
#' This includes validation of very basic data quality (large variance in gaze distribution, excessive blinks, large jumps in eye position, etc).
#' TODO: include functionality for logging of successes, warnings, failures. This will probably involve a trycatch statement that could handle a potentially large number of issues. We'll have to see how complicated it gets by balancing flexibility with parsimony. Tend to prefer flexibility if the package allows user-side functionality to be parsimonious :)
#' TODO: perhaps even store key variables (e.g. some measure of pupil fluctuation, or saccade velocity/acceleration) from prior subjects in separate circumscribed csv (which values get appended to) and plot distributions for every new subject. This would be akin to constructing a sort of empirical null distribution and performing informal (visual)"hypothesis tests" where we would hope certain variables in a given subject are not "significantly different" than the group distribution.
#' @param eye raw eye object pulled directly from the .edf file using read_edf(). Must be a list with expected_edf_fields c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file").
#' @importFrom lubridate seconds_to_period


ep.eye_initialize <- function(eye, 
                              expected_edf_fields = c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file"), 
                              task = NULL,
                              gaze_events = c("sacc", "fix", "blink"),
                              meta_check = NULL, 
                              header = "3. Initialize eye object:",
                              ...){

## debug
# expected_edf_fields = c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file")
# task = "neighborhood"
# gaze_events = c("sacc", "fix", "blink")
# meta_check = config$initialize_opts$meta_check

  if (class(eye) != "list") { stop("Something went wrong: initialize_eye requires list input.") }

  log_chunk_header(header)

  ### 3.1 make sure all names are present
  tryCatch.ep({
    stopifnot(all(expected_edf_fields %in% names(eye)))
  }, describe_text = "- 3.1 Check expected fields:")

  ### 3.2 initialize basic eye object structure
  tryCatch.ep({
    ep.eye <- ep.eye_setup_structure(eye, task = task)
  }, describe_text = "- 3.2 Initialize ep.eye list structure:")

  ### 3.3 document entire recording session length (if this is very different from BAU this should get flagged later)
  tryCatch.ep({
    ep.eye <- ep.eye_get_session_length(ep.eye)
  }, describe_text = paste0("- 3.3 Document recording session length (", seconds_to_period(round(ep.eye$metadata$recording_time)),"):", sep = ""))

  ### 3.4 check for continuity in timestamp on raw data
  tryCatch.ep({
    ep.eye <- ep.eye_raw_sample_continuity_check(ep.eye)
  }, describe_text = "- 3.4 Check for continuity in raw data:")

  ### 3.5 check for continuity in events (e.g. that events in time dont jump skip or go out of order)
  tryCatch.ep({
    # confirmed that unique sorts in order they appear in the array. E.g. y <- c(1,1,3,3,2,3); unique(y) : [1] 1 3 2.
    # will therefor check for skipped events and the ordering.
    stopifnot(all(unique(ep.eye$raw$eventn) == seq(min(unique(ep.eye$raw$eventn)), max(unique(ep.eye$raw$eventn)),1)))
  }, "- 3.5 Confirm raw event continuity:")

  ### 3.6 Unify gaze events.
  if(!is.null(gaze_events)){
    ### 6. check for matching between raw timestamps and saccades, fixations, blinks ("gaze events")
    cat(paste0("- 3.6 Unify gaze events(", paste0(gaze_events, collapse = ", "), ") and raw data:"))
    ep.eye <- ep.eye_unify_gaze_events(ep.eye, gaze_events = gaze_events)
  } else {
    "- 3.6 Unify gaze events: SKIP"
  }

  ### 3.7 Store between-event messages
  tryCatch.ep({
    ep.eye <- ep.eye_store_between_event_messages(ep.eye)
  }, "3.7 Store between-event messages:")

  ### 3.8 rm cr.info
  tryCatch.ep({
    ep.eye <- ep.eye_rm_crinfo(ep.eye)
  }, "3.8 Remove cr.info column from raw data:")

  ### 3.9 Unify et.msgs into raw data.
  tryCatch.ep({
    ep.eye <- ep.eye_unify_raw_msg(ep.eye)
  }, "3.9 Unify et.msgs into raw data:")

  ### 3.10 Check metadata
  dt <- "- 3.10 Check ep.eye metadata:"
  if("meta_check" %in% names(config$definitions$eye$initialize)){
    ep.eye <- ep.eye_meta_check(ep.eye, 
               meta_vars = config$definitions$eye$initialize$meta_check$meta_vars,
               meta_vals = config$definitions$eye$initialize$meta_check$meta_vals,
               recording_time = config$definitions$eye$initialize$meta_check$recording_time,
               dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 3.11 Shift timestamps to 0 start point
  ep.eye <- shift_eye_timing(ep.eye,"- 3.11 Shift timestamps to 0 start point:")

  cat("\n")
  return(ep.eye)
}

