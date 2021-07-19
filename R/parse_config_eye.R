# Parse task config file, extracting essential task information

#' @param eye ep.eye object that has been previously initialized and tidied
#' @param config list that is generated from reading task.yaml
#'


parse_config_eye <- function(eye, config, header = "3. Parse config file for ep.eye:") { #}, event_csv = NULL){#, .c = 3){
  # browser()
  tictoc::tic();
  # eye <- eye_init
  if (!"ep.eye" %in% class(eye)) { stop("parse_config_eye expects a pre-initialized ep.eye object") }

  log_chunk_header(header)

  ### 3.1 Extract eye definitions
  dt <- "- 3.1 Extract eye definitions for processing:"
  c.e <- tidy_eye_config(config, dt)

  ### 3.2 Check metadata
  dt <- "- 3.2 Check metadata:"
  if("meta_check" %in% names(c.e)){
    meta_check(c.e, eye, dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 3.3 Extract important between-trial messages if requested
  dt <- "- 3.3 Extract important between-trial messages:\n"
  if("inherit_btw_tr" %in% names(c.e)){
    eye <-  handle_between_trial(c.e, eye, dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 3.4 Extract event info
  dt <- "- 3.4 Parsing event information:\n"
  if("event_info" %in% names(c.e)){
    eye <- get_event_info(c.e, eye, dt)#, event_info)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 3.5 Event sequences check
  dt <- "- 3.5 Check event message sequence and ordering:"
  if("msg_seq" %in% names(c.e$event_info)){
    eye <- check_msg_seq(c.e, eye, dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }



  # ### 3.6 Collapse timestamps
  # dt <- "- 3.6 Collapse timestamps with more than one row:"
  # eye <- collapse_time(eye, dt)

  return(eye)
}


