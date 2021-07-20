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
#' @export

setup_proc_configs <- function(file,config_path){
  stopifnot(file.exists(file))
  if (length(file) > 1L) { stop("At present, read_process_eye is designed for one file at a time") }

  config <- validate_exp_yaml(config_path)
  
  ################### read processing options from config into environment
  opts <- config$definitions$eye$process_opts
  invisible(list2env(opts,  envir = environment()))

  ### Set prefix string
  if(exists("prefix")){
    if(is.null(prefix)) prefix <- str_extract(basename(file), "\\d{3}_[[:upper:]]+")  ## N.B. make more flexible if null
  } else {
    prefix <- str_extract(basename(file), "\\d{3}_[[:upper:]]+")  ## N.B. make more flexible if null 
  }
  opts[["prefix"]] <- prefix

  ### Setup ep.eye log: initialize log file if requested. Otherwise will print feedback while running checks.
  ## N.B. right now this will overwrite existing files, can come back to later.
  if(exists("gen_log")) {
      if(gen_log){
      log_dir <- config$definitions$eye$process_opts$log_dir
      init_eyelog(log_dir, prefix, file)   
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

## assign updated options into parent environment.
invisible(list2env(opts, envir = parent.frame()))


return(config)
}
