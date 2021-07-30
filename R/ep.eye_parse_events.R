#' @title Parse task config file, extracting essential task information

#' @param ep.eye ep.eye object that has been previously initialized and tidied
#' @param header  String for section header. Defaults to "4. Parse task events:"
#' 
#' @return ep.eye appended with important event-level information included.
#' 
#' @author Nate Hall
#' @expor`t


ep.eye_parse_events <- function(ep.eye, 
                                inherit_btw_ev = NULL,
                                config, 
                                header = "4. Parse task messages:") { 
  
  if (!"ep.eye" %in% class(eye)) { stop("parse_config_eye expects a pre-initialized ep.eye object") }

  log_chunk_header(header)

  ### 4.1 Extract important between-event messages if requested
  dt <- "- 4.1 Extract important between-event messages:\n"
  if(!is.null(inherit_btw_tr)){
    ep.eye <-  ep.eye_handle_between_event_msgs(ep.eye, 
                                                inherit_btw_ev, 
                                                dt)
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

  return(eye)
}


