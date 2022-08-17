############################
##### List of subsidiary functions utilized in `ep.phys_setup_proc_config()`
############################
# - ep.phys_set_config_definitions()
# -- ep.phys_set_config_definitions_helper()
# - ep.phys_build_ttl_seq()
############################


#' @title Add default definitions to config file
#' @description 
#' @param file Path to .edf file
#' @param config Named list extracted from config file
#' @param setup_config_fields list of characters of the fields in the config to setup. The available fields are "global", "initialize", "ecg_preproc", "eda_preproc", "qa"
#' 
#' @return conig. Named list with config file, including default values if missing from config
#' 
#' @author Nidhi Desai
#' 
#' @export
ep.phys_set_config_definitions <- function(file, config, setup_config_fields = NULL){
  
  if (is.null(setup_config_fields)){ setup_config_fields <- c("global", "initialize", "ecg_preproc", "eda_preproc", "qa") }
  
  for(f in setup_config_fields){
    config <- ep.phys_set_config_definitions_helper(file, config, f)
  }
  
 return(config)
}


#' @title Helper function to setup configuration file for a particular field.
#' @description 
#' @param file Path to .edf file
#' @param config Named list extracted from config file
#' @param field field/section of the config to setup. The available fields are "global", "initialize", "ecg_preproc", "eda_preproc", "qa"
#' 
#' @importFrom stringr str_extract 
#' 
#' @return Named list with config file, including default values if missing from config for one section/field of the config file
#' 
#' @author Nidhi Desai
ep.phys_set_config_definitions_helper <- function(file, config, field){

  # Global
  if (field == "global"){
    
    ## Extract from config or add default options
    if ("global" %in% names(config$definitions$physio)){
      opts <- config$definitions$physio$global
    } else {
      # if processing options are not specified, revert to default options.
      opts <- list()
      opts[["gen_log"]] <- TRUE
      opts[["save_preproc"]] <- TRUE
    }
    
    ## Set prefix string. If a regex string is provided, extract from file name otherwise set to the base file name.
    if ("prefix" %in% opts){
      if(!is.null(opts[["prefix"]])){
        opts$prefix <- str_extract(basename(file), opts[["prefix"]])
      } else {
        opts[["prefix"]] <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
      }
    } else {
      opts[["prefix"]] <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
    }
  
    ## Set subject ID if regex is provided.
    if ("subID" %in% opts){
      if(!is.null(opts[["subID"]])){
        opts[["subID"]] <- str_extract(basename(file), opts[["subID"]])
      } else {
        opts[["subID"]] <- NULL
      }
    } else {
      opts[["subID"]] <- NULL
    }
    
    ## Setup ep.physio log file. Initialize log file to save the information if requested. Otherwise will print feedback on console while running checks.
    if("gen_log" %in% opts){
      if(gen_log){
        if(!("log_dir" %in% opts)){
          opts[["log_dir"]] <- getwd()
        }
        init_phylog(file, opts[["log_dir"]], opts[["prefix"]])
      }
    }
    
    ## Setup folder to save preprocessed data: If none provided, creates directory "preproc" in working directory.
    # TODO add later if we need preproc in physio and in what format. Will this contain the functionality where the spliced data for each task be saved somewhere
    
  }
    
  # Initialize
  else if(field == "initialize"){
  
    ## Extract from config
    if ("initialize" %in% names(config$definitions$physio)){
      opts <- config$definitions$physio$initialize
      
      # TODO need to confirm if using ttl codes for checks and splicing will be skipped in ttl_codes_task is not mentioned in the config file

      ### extract information about ttl codes
      if ("ttl_codes_task" %in% names(opts)){
        opts[["ttl_codes"]] <- TRUE
        
        #### Note if the data needs to be spliced
        if ("task_start" %in% names(opts) & "task_end" %in% names(opts)){
          if (!is.numeric(opts[["task_start"]])){
            stop("initialize > ttl_codes_task > task_start is not numeric") # TODO change this to warning or error later 
          } else if (!is.numeric(opt[["task_end"]])){
            stop("initialize > ttl_codes_task > task_end is not numeric")
          } else {
            if (!("splice_data" %in% names(opts))){
              opts[["splice_data"]] <- TRUE
            }
          }
        }
      } else { # No ttl_codes_task option in config file
        opts[["ttl_codes"]] <- FALSE
        opts[["splice_data"]] <- FALSE
        warning("ttl_codes_task not mentioned in config file. Data splicing will not be performed")
      }
      
      #### meta_check will default to NULL and will be skipped in initialization procedure if not specified
    } 
    
    ### if processing options are not specified, revert to default options.
    else {
      opts[["splice_data"]] <- FALSE
      opts[["downsample_factor"]] <- NULL
      opts[["ttl_codes"]] <- FALSE
      opts[["ttl_codes_task"]] <- NULL
      opts[["meta_check"]] <- NULL
    }
  }
  
  # ECG preproc
  else if(field == "ecg_preproc"){
    # TODO Add more ECG inputs later
    
    ## Extract from config
    if ("ecg_preproc" %in% names(config$definitions$physio)){
      opts <- config$definitions$physio$ecg_preproc
      if ("spike_algorithm" %in% names(opts)){}
      if ("spike_call" %in% names(opts)){}
      
      if (!is.null(opts[["beat_detection"]]){
        if (is.null(opts[["beat_detection"]][["wfdb_path"]]){
          opts[["beat_detection"]][["wfdb_path"]] <- "/usr/local/wfdb/bin" # TODO confirm if this should be the default value
        }
        if (is.null(opts[["beat_detection"]][["beats_detector"]])){
          opts[["beat_detection"]][["beats_detector"] <- c("wqrs", "gqrs", "ecgpuwave", "sqrs")
        }
      } else {
        opts[["beat_detection"]][["wfdb_path"]] <- "/usr/local/wfdb/bin"
        opts[["beat_detection"]][["beats_detector"] <- c("wqrs", "gqrs", "ecgpuwave", "sqrs")
      }

    } else { # if ecg_preproc is not mentioned then ecg preprocessing won't be done
      opts <- FALSE # TODO should we have this FALSE or NULL. Is this assumption correct that if ecg_preproc is not mentioned then ecg data should not be processed 
    }
  }
  
  # EDA preproc
  else if(field == "eda_preproc"){
    # TODO add eda preproc inputs later
    if ("eda_preproc" %in% names(config$definitions$physio)){
      opts <- config$definitions$physio$eda_preproc
      
      if ("decomposition" %in% names(opts)){
        if (!"tau" %in% names(opts[["decomposition"]])) { opts[["decomposition"]][["tau"]] <- } # TODO add a default tau value
        if (!"scl_range" %in% names(opts[["decomposition"]])) { opts[["decomposition"]][["scl_range"]] <- c(2, 20)} # in microsiemens (uS)
        if ("scr" %in% names(opts[["decomposition"]])){ 
          if (!"amp_threshold" %in% names(opts[["decomposition"]][["scr"]])) { opts[["decomposition"]][["scr"]][["amp_threshold"]] <- 0.01 } # in muS
          if (!"response_window" %in% names(opts[["decomposition"]][["scr"]])) { opts[["decomposition"]][["scr"]][["response_window"]] <- c(1, 4) } # in seconds
        } else {
          opts[["decomposition"]][["scr"]][["amp_threshold"]] <- 0.01 # in muS
          opts[["decomposition"]][["scr"]][["response_window"]] <- c(1, 4) # in seconds
        }
      } else {
          opts[["decomposition"]][["tau"]] <-  # TODO add a default tau value
          opts[["decomposition"]][["scl_range"]] <- c(2, 20) # in microsiemens (uS)
          opts[["decomposition"]][["scr"]][["amp_threshold"]] <- 0.01 # in muS
          opts[["decomposition"]][["scr"]][["response_window"]] <- c(1, 4) # in seconds
      }
    } else {
      opts <- FALSE # TODO should we have this FALSE or NULL. Is this assumption correct that if ecda_preproc is not mentioned then eda data should not be processed
    }
  }
  
  # QA
  else if(field == "qa"){
    
    # TODO left here
    
  }
  
  config[["definitions"]][["physio"]][[field]] <- opts
  return(config)
}


#' @title Build out expected ttl code eval_sequence from config file
#' @description read in config file and process the blocks section so that for each ttl_code mentioned, this function outputs a tibble with columns as ttl_code (parport_code), stimuli (event in config), phase and expected_freq (ntrials in config). This table will be used in the argument code_labels_df in augment_ttl_details.
#' @details 
#'   The ttl_codes which should be send in the middle of the task_start and task_end codes are noted in ttl_codes_task$all_mid_codes.
#'   This function will be run only if initialize$ttl_codes_task$eval_middle is set to TRUE.
#' @param config Named list extracted from config file
#' 
#' @import tibble tibble
#' @import stringr str_extract
#' @import dplyr select %>%
#' 
#' @return tibble with ttl information with each row contains information regarding one ttl code that could be sent with the physio data and the columns contain the ttl_code, the stimuli and phase that this ttl_code will be sent in and expected frequency of this ttl_code 
#' 
#' @author Nidhi Desai, Nila Thillaivanan
#' 
#' @export
# TODO Should we add code to create the eval_sequence in which the codes will be sent? Mostly yes create a list of expected eval_sequence of ttl codes
ep.phys_build_ttl_seq <- function(config){
  
  # (1) Extract phase and stimuli name for ttl_code and expected frequency from blocks section of config file
  
  # unlist the yaml blocks to access names more easily
  unlisted_blocks <- unlist(config[["blocks"]], recursive = TRUE)
  block_names <- names(unlisted_blocks)
  ttl_blocks_info <- tibble(
    ttl_code = numeric(),
    stimuli = character(),
    phase = character(),
    expected_freq = numeric())
  
  ttl_indx <- which(grepl(".ttl_code", block_names))
  for (i in ttl_indx){
    
    # check if the ttl_code mentioned in the config are non-numeric
    if (grepl("\\D", unlisted_blocks[[i]])) { stop(paste(unlisted_blocks[[i]], "ttl_code is non-numeric")) }
    
    expected_freq <- unlisted_blocks[[paste0(str_extract(names(unlisted_blocks[i]), "^\\w+"), '.', "ntrials")]]
    if (grepl("\\D", expected_freq)) { stop(paste("ntrials:", expected_freq, "is non-numeric")) }
    
    # TODO current this only works if one ttl_code in inside only one phase and one event. We might need to extend this code
    # such that if the same ttl_code appears multiple times, it gets noted somehow and is taken into the frequency calculation.
    ttl_blocks_info <- ttl_blocks_info %>% add_row(ttl_code = as.numeric(unlisted_blocks[[i]]),
                                     stimuli = sub(".events.", "", str_extract(names(unlisted_blocks[i]), ".events.\\w+")),
                                     phase = str_extract(names(unlisted_blocks[i]), "^\\w+"),
                                     expected_freq = as.numeric(expected_freq))
  }
  config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["info"]] <- ttl_blocks_info %>% select(-expected_freq)
  
  config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["expected_freq"]] <- ttl_blocks_info %>% select(ttl_code, expected_freq)

  
  # (2) Extract the expected eval_sequence of ttl_codes from the blocks section of config file
  # TODO add code for building ttl_code expected eval_sequence
  # config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["expected_seq"]] 
  
  if (!is.null(config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["eval_sequence"]])){ # in not null
    if (config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["eval_sequence"]]){ # if eval_sequence is TRUE
      # only if these conditions are satisfied, create an expected eval_sequence of ttl_codes
      # TODO add code here so that it works for simple cases like dtk pav and pit phase
      
      for (i in ttl_indx){
        # check if the ttl_code mentioned in the config are non-numeric
        if (grepl("\\D", unlisted_blocks[[i]])) { stop(paste(unlisted_blocks[[i]], "ttl_code is non-numeric")) }
        
      }
    }
  }
  
  return(config)
}







