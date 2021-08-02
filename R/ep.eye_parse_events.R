#' @title Parse task config file, extracting essential task information

#' @param ep.eye ep.eye object that has been previously initialized and tidied
#' @param header  String for section header. Defaults to "4. Parse task events:"
#' 
#' @return ep.eye appended with important event-level information included.
#' 
#' @author Nate Hall
#' @export


ep.eye_parse_events <- function(ep.eye, 
                                inherit_btw_ev = NULL,
                                event_info = NULL, 
                                prefix = NULL,
                                header = NULL) { 
  
  if (!"ep.eye" %in% class(ep.eye)) { stop("parse_config_eye expects a pre-initialized ep.eye object") }

  if(!is.null(header)) log_chunk_header(header)

  ### 4.1 Extract important between-event messages if requested. N.B. 4.1.2 move_to_within still needs work!!!
  dt <- "- 4.1 Extract important between-event messages:\n"
  if(!is.null(inherit_btw_ev)){
    ep.eye <-  ep.eye_handle_between_event_msgs(ep.eye, 
                                                inherit_btw_ev, 
                                                dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 4.2 Extract event info
  dt <- "- 4.2 Parsing event information:\n"
  if(!is.null(event_info)){
    ep.eye <- ep.eye_parse_event_info(ep.eye, 
                                 extraction_method = event_info$extraction_method,
                                 extract_event_func_path = event_info$extract_event_func_path,
                                 csv_path = event_info$csv_path,
                                 prefix = prefix,
                                 msg_seq = event_info$msg_seq,
                                 dt = dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 4.3 Event sequences check
  dt <- "- 4.3 Validate message sequence and ordering:\n"
  if(!is.null(event_info$msg_seq)){
    ep.eye <- ep.eye_validate_msg_seq(ep.eye, 
                                      msg_seq = event_info$msg_seq,
                                      dt = dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  return(ep.eye)
}


