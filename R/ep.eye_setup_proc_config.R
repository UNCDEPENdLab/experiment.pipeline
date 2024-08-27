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

ep.eye_setup_proc_config <- function(edf_raw,
                                     config_path,
                                     header = NULL){
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
  stopifnot(file.exists(edf_raw) & file.exists(config_path))
  if (length(edf_raw) > 1L) { stop("At present, ep.eye_setup_proc_configs is designed for one file at a time") }

  #custom recursive function (will allow for nesting in specified lists)
  merge_configs <- function(user_config, default_config) {
    if (is.null(default_config)) {
      return(user_config)
    }

    for (name in names(default_config)) {
      if (is.list(default_config[[name]])) {
        if (name %in% names(user_config) && is.list(user_config[[name]])) {
          # Recursively merge nested lists
          default_config[[name]] <- merge_configs(user_config[[name]], default_config[[name]])
        }
      } else {
        # Replace default with user value if it exists
        if (name %in% names(user_config)) {
          default_config[[name]] <- user_config[[name]]
        }
      }
    }

    # Add any new elements from user_config that aren't in default_config
    for (name in setdiff(names(user_config), names(default_config))) {
      default_config[[name]] <- user_config[[name]]
    }

    return(default_config)
  }

  # Load your configurations
  config_user <- validate_exp_yaml(config_path)
  config_defaults <- ep.eye_default_options(edf_raw)

  # Definitions you care about
  eye_proc_definitions <- c("global", "initialize", "msg_parse", "gaze_preproc", "pupil_preproc", "qa")

  # Check for typos or unexpected sections
  stopifnot(all(names(config_user$definitions$eye) %in% eye_proc_definitions))

  # Initialize the final config
  config <- list()

  # Loop over each eye_proc_definition
  for (nit in eye_proc_definitions) {
    # Get user and default options for the current section
    user_opts <- config_user$definitions$eye[[nit]]
    default_opts <- config_defaults[[nit]]

    # Merge the options using the recursive function
    merged_opts <- merge_configs(user_opts, default_opts)

    # Assign the merged configuration back to the final config list
    config$definitions$eye[[nit]] <- merged_opts
  }

  # pull id from prefix
  config$definitions$eye$global$id <- str_extract(edf_raw, config$definitions$eye$global$id)

  #### Build block and event-specific message sequences.
  config[["definitions"]][["eye"]] <- config %>% ep.eye_build_msg_seq(dt = paste0("- Build block/event-specific expected message sequences:"))

   bannerCommenter::boxup("Final ep.eye Config Specification:") |> cat()
  for(nit in eye_proc_definitions){
    cat(paste0("------ ",nit, " config options:\n"))
    if(!is.null(config$definitions$eye[[nit]])){
      nate.utils::print_tree(config$definitions$eye[[nit]]); if(nit == "global") cat("\n") # this just makes the spacing a little prettier
    } else {
      cat(paste0("└─", nit, " definitions NULL [empty] with no default\n\n"))
    }
  }

  return(config)
}

