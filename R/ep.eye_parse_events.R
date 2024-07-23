#' @title Parse task config file, extracting essential task information

#' @param ep.eye ep.eye object that has been previously initialized and tidied
#' @param extract_event_func_path Path to user-defined message parsing function here
#' @param csv_path Path to write event .csvs to
#' @param msg_seq List of optional message sequence arguments passed in config file. Can contain \code{msg_start}, \code{msg_end}, \code{eval_middle}, \code{ordered}. See the ep.eye_config vignette for details on these fields.
#' @param header  String for section header. Defaults to NULL
#'
#' @return ep.eye appended with important event-level information included.
#'
#' @author Nate Hall
#' @export


ep.eye_parse_events <- function(ep.eye,
                                extract_event_func_path,
                                csv_path,
                                msg_seq,
                                header = NULL) {
  # browser()
  # debug:
  # -----
  # ep.eye <- eye_init
  # extract_event_func_path = config$definitions$eye$msg_parse$extract_event_func_path
  # csv_path = file.path(config$definitions$eye$msg_parse$csv_dir_path, paste0(config$definitions$eye$global$prefix, ".csv"))
  # msg_seq = config$definitions$eye$msg_parse$msg_seq
  # header = "3. Parse task events:"
  # -----


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

  ### save initialized ep.eye object to correct folder
  if(config$definitions$eye$global$save_steps){
    parse_dir <- config$definitions$eye$global$preproc_out %>% file.path(., "ep.eye_parse_events")
    if(!dir.exists(parse_dir)) {dir.create(parse_dir, recursive = TRUE)}
    subj_path <- file.path(parse_dir, paste0(config$definitions$eye$global$id, ".rds"))
    tryCatch.ep({
      saveRDS(ep.eye, subj_path)
    },
    describe_text = paste0("- 3.3 Save ep.eye with events parsed [", subj_path,"]:"))
  }

  return(ep.eye)
}


