#' @title Parse task config file, extracting essential task information

#' @param ep.eye ep.eye object that has been previously initialized and tidied
#' @param header  String for section header. Defaults to "4. Parse task events:"
#' 
#' @return ep.eye appended with important event-level information included.
#' 
#' @author Nate Hall
#' @export


ep.eye_parse_events <- function(ep.eye, 
                                extraction_method,
                                extract_event_func_path,
                                csv_path, 
                                msg_seq,
                                header = NULL) { 
  # debug
  # ep.eye <- eye_init
  # extraction_method = config$definitions$eye$msg_parse$extraction_method
  # extract_event_func_path = config$definitions$eye$msg_parse$extract_event_func_path
  # csv_path = file.path(config$definitions$eye$msg_parse$csv_dir_path, paste0(config$definitions$eye$global$prefix, ".csv"))
  # msg_seq = config$definitions$eye$msg_parse$msg_seq
  # header = "3. Parse task events:"

  
  if (!"ep.eye" %in% class(ep.eye)) { stop("parse_config_eye expects a pre-initialized ep.eye object") }

  if(!is.null(header)) log_chunk_header(header)

  ### 3.1 Extract event info
  ep.eye <- ep.eye_parse_event_info(ep.eye, 
                                    extract_event_func_path = extract_event_func_path,
                                    csv_path = csv_path,
                                    msg_seq = msg_seq,
                                    dt = "- 3.1 Parsing event information:\n")

  ### 3.2 Event sequences check
  dt <- "- 3.2 Validate message sequence and ordering:\n"
  if(!is.null(msg_seq)){
    ep.eye <- ep.eye_validate_msg_seq(ep.eye, 
                                      msg_seq = msg_seq,
                                      dt = dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  return(ep.eye)
}


