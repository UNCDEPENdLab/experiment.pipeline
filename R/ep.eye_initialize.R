#' generic function for initializing ep.eye object and performing basic internal checks on the eye data, while remaining agnostic to task/behavior structure.
#'
#' This includes validation of very basic data quality (large variance in gaze distribution, excessive blinks, large jumps in eye position, etc).
#' TODO: include functionality for logging of successes, warnings, failures. This will probably involve a trycatch statement that could handle a potentially large number of issues. We'll have to see how complicated it gets by balancing flexibility with parsimony. Tend to prefer flexibility if the package allows user-side functionality to be parsimonious :)
#' TODO: perhaps even store key variables (e.g. some measure of pupil fluctuation, or saccade velocity/acceleration) from prior subjects in separate circumscribed csv (which values get appended to) and plot distributions for every new subject. This would be akin to constructing a sort of empirical null distribution and performing informal (visual)"hypothesis tests" where we would hope certain variables in a given subject are not "significantly different" than the group distribution.
#' @param eye raw eye object pulled directly from the .edf file using read_edf(). Must be a list with expected_edf_fields c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file").


ep.eye_initialize <- function(eye, 
                              expected_edf_fields = c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file"), 
                              task = NULL,
                              gaze_events = c("sacc", "fix", "blink"),
                              meta_check = NULL, ...){
expected_edf_fields = c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file")
task = "neighborhood"
gaze_events = c("sacc", "fix", "blink")
meta_check = config$initialize_opts$meta_check

  if (class(eye) != "list") { stop("Something went wrong: initialize_eye requires list input.") }

  # cat("\n--------------\n", c., " Initialize eye object:\n--------------\n")
  cat("\n--------------\n2. Initialize eye object:\n--------------\n")

  ### 2.1 make sure all names are present
  tryCatch.ep({
    stopifnot(all(expected_edf_fields %in% names(eye)))
  }, describe_text = "- 2.1 Check expected fields:")

  ### 2.2 initialize basic eye object structure
  tryCatch.ep({
    ep.eye <- ep.eye_setup_structure(eye, task = task)
  }, describe_text = "- 2.2 Initialize ep.eye list structure:")

  ### 2.3 document entire recording session length (if this is very different from BAU this should get flagged later)
  tryCatch.ep({
    ep.eye <- ep.eye_get_session_length(ep.eye)
  }, describe_text = paste0("- 2.3 Document recording session length (", lubridate::seconds_to_period(round(ep.eye$metadata$recording_time)),"):", sep = ""))

  ### 2.4 check for continuity in timestamp on raw data
  tryCatch.ep({
    ep.eye <- ep.eye_raw_sample_continuity_check(ep.eye)
  }, describe_text = "- 2.4 Document missing measurements:")

  ### 2.5 check for continuity in events
  tryCatch.ep({
    # confirmed that unique sorts in order they appear in the array. E.g. y <- c(1,1,3,3,2,3); unique(y) : [1] 1 3 2.
    # will therefor check for skipped events and the ordering.
    stopifnot(all(unique(ep.eye$raw$eventn) == seq(min(unique(ep.eye$raw$eventn)), max(unique(ep.eye$raw$eventn)),1)))
  }, "- 2.5 Confirm raw event continuity:")

  ### 2.6 Unify gaze events.
  if(!is.null(gaze_events)){
    ### 6. check for matching between raw timestamps and saccades, fixations, blinks ("gaze events")
    paste0("- 2.6 Unify gaze events(", paste0(gaze_events, collapse = ", "), ") and raw data:")
    ep.eye <- ep.eye_unify_gaze_event(ep.eye, gaze_events = gaze_events)
  } else {
    "- 2.6 Verify correspondence of gaze events and raw data: SKIP"
  }



  ### 8. Finish up tidying of raw DT: 1) store messages with no timestamp 2) verify uselessness of cr.info column 3) merge with messages, and "back-check" for errors.

  cat("- 2.8 Finish raw DT tidying:\n", sep = "")

  # 8.1 store messages with no timestamp match in raw data (collected between trials with no corresponding measurements).
  # In my (neighborhood) checks these have to do with calibration parameters, display coords, etc. For most users this will not be very helpful.
  # N.B. however, if the user passes important information before turning the tracker on (as in the sorting mushrooms data), it will be important to allow for users to move messages in the interstitial spaces between recordings to the beginning of a trial/event. Later will include this in the YAML parsing framework.
  btw_tr <- eye$msg %>% anti_join(eout$raw, by = "time") %>% data.table()
  if(nrow(btw_tr) == 0){
    cat("-- 2.8.1 Between-trial message storage: COMPLETE (EMPTY)\n", sep = "")
  } else{
    eout$metadata[["btw_tr_msg"]] <- btw_tr
    cat("-- 2.8.1 Between-trial message storage: COMPLETE (NON-EMPTY)\n", sep = "")
  }

  # 8.2 drop cr.info column
  cr <- unique(eout$raw$cr.info)
  if(length(cr) == 1 & cr == "..."){
    eout$raw <- eout$raw %>% select(-cr.info)
    cat("-- 2.8.2 Drop cr.info in raw data: COMPLETE\n", sep = "")
  } else{ cat("-- 2.8.2 Retain cr.info in raw data: COMPLETE (",paste0(cr, collapse = ","),")\n", sep = "")}

  # 8.3 merge messages to raw data.
  # N.B. the left_join means that between trial messages will not be copied over but rather are stored in metadata if between trial messages are of interest. Since there is no corresponding measurements of gaze/pupil in the raw data there is nowhere in the raw time series to place these relatively unhelpful messages.
  # N.B. Under the current set-up this operation will increase the number of rows if multiple messages are passed simultaneously. At a later point, one could change this format with the yaml config file under $definitions$eye$coincident_msg.
  eout$raw <- eout$raw %>% left_join( dplyr::filter(eye$msg, !text %in% unique(btw_tr$text)), by = c("eventn", "time")) %>% rename(`et.msg` = `text`)  %>% mutate(et.msg = ifelse(is.na(et.msg), ".", et.msg)) %>% data.table()

  # important to back-translate to original messages due to the use of left_join.
  umsg <- unique(eout$raw$et.msg)[which(unique(eout$raw$et.msg) != ".")] #unique messages in the final output.

  if(nrow(btw_tr) != 0){
    umsg_edf <- unique(eye$msg$text) # unadulterated, right off the eyetracker.
    umsg_orig <- umsg_edf[which(!umsg_edf %in% unique(btw_tr$text))] # make sure to eliminate between-trial messages and just grab messages that are passed while recording.

    # length(umsg) + length(unique(btw_tr$text)) == length(unique(eye$msg$text))
  } else{
    umsg_orig <- eye$msg$text # no btwn-trial messages to filter out.
  }

  if(all(umsg %in% umsg_orig)){
    cat("-- 2.8.3 Merge raw gaze data with eyetracker messages, with successful back-translate: COMPLETE\n", sep = "")
  } else{ # if any mismatch between what is contained in raw data and original message structure, print error and do some digging.

    miss_msgs <- umsg[!umsg %in% umsg_orig]
    eout$metadata[["missing_messages_raw"]] <- miss_msgs

    mmsgs_stamped <- eye$msg[which(eye$msg$text %in% miss_msgs), ]
    for(i in 1:nrow(mmsgs_stamped)){
      mstamp <- mmsgs_stamped[i,2] #grab missing timestamp.
      mstamp %in% mm
      mm
    }


    cat("-- 2.8.3 Merge raw gaze data with eyetracker messages, with successful back-translate: WARNING: errors in this step have not been fully vetted. \n", sep = "")
  }


 ### 9 Check metadata
  dt <- "- 2.9 Check metadata:"
  if("meta_check" %in% names(c.e)){
    meta_check(c.e, eye, dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 10 Shift timestamps to 0 start point
  dt <- "- 2.10 Shift timestamps to 0 start point:"
  eout <- shift_eye_timing(eout,dt)


  cat("\n")
  return(eout)
}

