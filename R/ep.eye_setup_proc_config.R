#' @title Read and setup processing configuration
#' @description Validates and imports processing configuration options into the environment for later use. Implements default configuration options if missing from YAML.
#' @param file Path to the .edf file to process.
#' @param config_path Path to corresponding .yml configuration file with processing instructions. Instructions on how to effectively set up a configuration file can be found [HERE].
#' @return Nested list with processing options pulled from .yml configuration file.
#' @details  Also generates new log file using \code{sink()}, and sets  up a new directory for preprocessed output if it does not already exist.
#' @examples
#'  \dontrun{
#'    config <- setup_proc_configs("/proj/mnhallqlab/studies/NeuroMAP/s3_data/Neighborhood_PSU/eye/002_HS_Neighborhood_Eye.edf", "/proj/mnhallqlab/studies/NeuroMAP/s3_data_ep_specs/yaml/Neighborhood_PSU.yaml")
#'  }
#' @author Nate Hall
#'
#' importFrom stringr str_extract
#'
#' @export

ep.eye_setup_proc_config <- function(file, config_path, header = NULL){
  stopifnot(file.exists(file))
  if (length(file) > 1L) { stop("At present, ep.eye_setup_proc_configs is designed for one file at a time") }

  config <- validate_exp_yaml(config_path)

  #### start by pulling in global options which will launch an .elog file if requested.
  config <- ep.eye_set_config_definitions(file, config, "global")
  if(!is.null(header)){
    log_chunk_header(header)
    cat(paste0(str_extract(header, "\\d+\\."), "1 Setup global options: SUCCESS\n------ global config options:"))
    ep.list.tree(config$definitions$eye$global)
    cat("------\n")
  }

  # Designated definition fields. Implement defaults if no options are supplied.
  eye_proc_fields <- c("initialize", "msg_parse", "gaze_preproc", "pupil_preproc", "qa")
  count <- 1
  for(f in eye_proc_fields){
    count <- count + 1
    tryCatch.ep({
      config <- ep.eye_set_config_definitions(file, config, f)
    }, describe_text = paste0(str_extract(header, "\\d+\\."), count, " Setup ", f, " options:"))

    cat(paste0("------ ",f, " config options:"))
    ep.list.tree(config$definitions$eye[[f]])
    cat("------\n")

  }

  #### Build block and event-specific message sequences.
  config[["definitions"]][["eye"]] <- config %>% ep.eye_build_msg_seq(dt = paste0("1.", count+1, " Build block/event-specific expected message sequences:"))

return(config)
}

