# Importer for configurations
#
# Imports parameters to NeuroMap environment.

# Adds global envir
utils::globalVariables(c("NeuroMap"))

#' Read YAML Config File
#'
#' Read YAML configuration file defined.
#' All YAML document objects are returned as a list.
#' Configurations can be used to defined different configurations
#' from the YAML file.
#' All output is saved to global parameter in NeuroMap environment: CONFIG
#'
#' @param file YAML file path
#' @param configuration (default = "default") Configuration of YAML to read
#'
#' @return list of YAML configurations
#'
#' @export
import.yaml <- function(file, configuration = "default") {
  Sys.setenv(R_CONFIG_ACTIVE = configuration)
  config <- config::get(file = file)

  if((!exists("NeuroMap"))) {
    assign("NeuroMap", new.env(), envir = .GlobalEnv)
  }
  assign("CONFIG", config, envir = NeuroMap)

  # FDBeye Options
  options(FDBeye_edf2asc_opts = " -y")  # Overwrite Existing Files

  return(config)
}

#' Get System Info
#'
#' Retrieves system info defined by R.
#' Info includes:
#' - Operating System (OSX or Windows)
#'
#' All information is saved to global param on NeuroMap environment: SYSTEM
#'
#' @export
get.system <- function() {
  info <- utils::sessionInfo()

  if(grepl('mac', info$running, ignore.case = TRUE)) {
    system <- list(os = "OSX")
  } else if(grepl('win', info$running, ignore.case = TRUE)) {
    system <- list(os = "Windows")
  } else {
    log.error("Only Mac OSX and Windows are currently supported.")
    blank.msg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
    stop(simpleError(blank.msg))
  }

  if((!exists("NeuroMap"))) {
    assign("NeuroMap", new.env(), envir = .GlobalEnv)
  }

  assign("SYSTEM", system, envir = NeuroMap)
}
