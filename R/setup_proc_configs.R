#' @title Read and setup processing configuration
#' @description Validates and imports processing configuration options into the environment for later use.
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

setup_proc_configs <- function(file,config_path){
  stopifnot(file.exists(file))
  if (length(file) > 1L) { stop("At present, read_process_eye is designed for one file at a time") }

  config <- validate_exp_yaml(config_path)
  
  ################### read processing options from config into environment
  proc_opt_names <- c("prefix", "gen_log", "log_dir", "preproc_out", "return_raw")
  
  if("global_opts" %in% names(config$definitions$eye)){
    opts <- config$definitions$eye$global_opts
  } else{ # if processing options are not specified, revert to default options.
    opts <- list()
    opts[["prefix"]] <- NULL
    opts[["gen_log"]] <- TRUE
    opts[["log_dir"]] <- NULL
    opts[["save_preproc"]] <- TRUE
    opts[["preproc_out"]] <- NULL
    opts[["return_raw"]] <- FALSE
  }
  
  invisible(list2env(opts,  envir = environment()))


  ###################

  ### Set prefix string. If a regex string is provided, extract from file name otherwise set to the base file name. 
  if(exists("prefix")){
    if(!is.null(prefix)) {
      # prefix <- "\\d{3}_[[:upper:]]+"
      prefix <- str_extract(basename(file), prefix)  
    } else {
      prefix <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
    }
  } else {
    prefix <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
  }
  opts[["prefix"]] <- prefix

  ### Setup ep.eye log: initialize log file if requested. Otherwise will print feedback while running checks.
  ## N.B. right now this will overwrite existing files, can come back to later.
  if(exists("gen_log")) {
      if(gen_log){
      if(exists("log_dir")) {
        log_dir <- config$definitions$eye$global_opts$log_dir
      } else{log_dir <- getwd()}
      init_eyelog(file, log_dir, prefix)   
     }
  }
  

  ### Setup folder to save preprocessed data: If none provided, creates directory "preproc" in working directory.
  if(exists("preproc_out")){
     if(!is.null(preproc_out)) {
         if(!dir.exists(preproc_out)) dir.create(preproc_out, recursive = TRUE)
     } else {
     dir.create("preproc")
     preproc_out <- "preproc"
     } 
  } else {
     dir.create("preproc")
     preproc_out <- "preproc"
  }

  opts[["preproc_out"]] <- preproc_out

  ### Return raw data?
  if(!exists("return_raw")) opts[["return_raw"]] <- FALSE

## assign updated options into parent environment.
invisible(list2env(opts, envir = parent.frame()))

return(config)
}
