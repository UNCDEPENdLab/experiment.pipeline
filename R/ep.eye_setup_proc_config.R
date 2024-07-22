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
#' importFrom yaml read_yaml
#' importFrom nate.utils print_tree
#'
#' @export

ep.eye_setup_proc_config <- function(edf_raw, config_path, header = NULL){
  # debug:
  # -----
  # edf_raw <- "~/Documents/github_repos/arl_repos/dimt_analysis/data_raw/eye/dimt/595.edf"
  # config_path <- "~/Documents/github_repos/arl_repos/dimt_analysis/config/dimt_eye_config.yaml"
  # header = c("1. Setup Processing Options:",
  #            "-----------------------------",
  #            "- Review this output carefully!",
  #            "- Every ep.eye processing function takes config as an input.",
  #            "- Thus, config controls the behavior of every part of data processing moving forward!")
  # -----
  stopifnot(file.exists(edf_raw))
  if (length(file) > 1L) { stop("At present, ep.eye_setup_proc_configs is designed for one file at a time") }

  config <- validate_exp_yaml(config_path)

  #### start by pulling in global options which will launch an .elog file if requested.
  # TODO add verbose argument to suppress output (perhaps just default to verbose = FALSE, unless log == TRUE).
  config <- ep.eye_set_config_definitions(edf_raw, config, "global")
  if(!is.null(header)){
    bannerCommenter::open_box(header) %>% cat()
    cat(paste0("Config status: SUCCESS\n\n------- global config options:\n")) %>% cat()
    nate.utils::print_tree(config$definitions$eye$global)
    # cat("------\n")
  }

  # Designated definition fields. Implement defaults if no options are supplied.
  eye_proc_fields <- c("initialize", "msg_parse", "gaze_preproc", "pupil_preproc", "qa")
  # count <- 1
  for(f in eye_proc_fields){
    # count <- count + 1

    tryCatch.ep({
      config <- ep.eye_set_config_definitions(edf_raw, config, f)
    # }, describe_text = paste0(str_extract(header, "\\d+\\."), count, " Setup ", f, " options:"))
  })#, describe_text = paste0(" Setup ", f, " options:"))




    cat(paste0("------ ",f, " config options:\n"))
    if(!is.null(config$definitions$eye[[f]])){
      nate.utils::print_tree(config$definitions$eye[[f]])
    } else {
      cat(paste0("└─", f, " definitions NULL [empty] with no default\n\n"))
    }

  }

  #### Build block and event-specific message sequences.
  config[["definitions"]][["eye"]] <- config %>% ep.eye_build_msg_seq(dt = paste0("- Build block/event-specific expected message sequences:"))

return(config)
}

