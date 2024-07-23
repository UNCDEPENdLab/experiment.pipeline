###### this will process all .edf files in a single directory.
###### Assumes that files are structured in a similar way, if not, this will be reflected in the .elog files that are generated when running a subject through the processing procedure.
process_eye_dir <- function(dir, # directory with all edf files. Should be labeled in a way that a unique regular expression can extract identifying info, otherwise the exact file nae will be used as a base for rendering all subsequent file names
                            yaml_file, # path to yaml
                            ncores = 1, # to aid parallelization
                            id = NULL, # if a string is passed, will use str_extract to pull this information from the edf path
                            log = TRUE,
                            log_dir = NULL,
                            save_steps = TRUE,
                            out_dir = NULL,
                            event_info = NULL  ###### if event_info$extraction_method is data.frame, pass a function that will generate a data.frame. If csv, pass path to predefined .csv.
                            ){
  require(parallel)
  require(doSNOW)

  # testing
  # yaml <- "inst/examples/yaml_config/neighborhood.yaml"
  # dir <- paste0("NH_local/neuromap_processing/neighborhood/neighborhood_edf/")
  source("NH_local/setup_envi.R")

  config <- validate_exp_yaml(yaml_file = yaml_file)

  allFiles <- list.files(dir)

  edf_paths <- c()
  for(f in allFiles){
    edf_paths <- c(edf_paths, normalizePath(file.path(dir, f)) )
  }


  if(ncores > 1){
    cl <- makeCluster(ncores)
    registerDoSNOW(cl)
  } else{
    registerDoSEQ()
  }

  proc_edfs <- list()

  proc_edfs <- foreach(sub = edf_paths, .packages = names(sessionInfo()$otherPkgs)) %dopar% {
    id <- str_extract(sub, "\\d{3}")

    # if(config$definitions$eye$event_info$extraction_method %in% c("csv", "data.frame")){
    #   stopifnot(is.null(event_info)) # need to point to function or .csv file
    #   if
    # }


    out <- read_process_eye(file = sub, config = config, prefix = id,
                            log = log, log_dir = log_dir,
                            save_steps = save_steps, out_dir = out_dir)
    out
  }

  stopCluster(cl)
  return(proc_edfs)
}
