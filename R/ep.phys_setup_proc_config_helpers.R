############################
##### List of subsidiary functions utilized in `ep.phys_setup_proc_config()`
############################
# - ep.phys_set_config_definitions()
# -- ep.phys_set_config_definitions_helper()
# - ep.phys_set_config_expstruct()
# - ep.phys_get_ttl_freq()
# - ep.phys_build_ttl_seq()
############################
# TODO add testing of blocks section somewhere

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



#' @title 
#' @description 
#' @param 
#' 
#' @importFrom 
#' 
#' @return 
#' 
#' @author Nidhi Desai
ep.phys_set_config_expstruct <- function(config){
  # TODO we may or maynot need this function after the global read_config_expstruct() is done
  config <- read_config_expstruct(config$exp_structure)
  return(config)
}





#' @title Get the expected frequency of each ttl_code calculated from the blocks section of the config file
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
#' @return expected frequency of each ttl_code is added to config$definitions$physio$initialize$ttl_codes_task$expected_freq. A tibble with ttl information is added to config$definitions$physio$initialize$ttl_codes_task$info with each row containing information regarding one ttl code that could be sent with the physio data and the columns contain the ttl_code, the corresponding stimuli and phase.
#' 
#' @author Nidhi Desai, Nila Thillaivanan
#' 
#' @export
ep.phys_get_ttl_freq <- function(config){
  # Extract phase and stimuli name for ttl_code and expected frequency from blocks section of config file
  
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

  return(config)
}


#' @title Build out expected ttl code sequence based on the config file
#' @description Build out expected sequence of ttl codes based on phase and event sequence from the blocks section of the config file. Read in config file and process the blocks section such that the functions creates a list of expected ttl codes in the sequence that would be expected from the actual data. 
#'  This is created using the sequence in which blocks/phases and events are defined in the blocks section of the config file.
#' @param config Named list extracted from config file
#' 
#' @return config structure with a list of ttl_codes in their expected sequence added to config$definitions$physio$initialize$ttl_codes_task$expected_seq
#' 
#' @author Nidhi Desai
#' 
#' @export
ep.phys_build_ttl_seq <- function(config){
  # TODO this function needs to be generalized to blocks format for different types of event sequences
  # NOTE right now this function works for simple cases like dtk pav and pit phase and not dtk instrumental phase
  
  # only if eval_sequence is not null and is TRUE, create an expected expected_seq of ttl_codes
  if (!is.null(config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["eval_sequence"]])){
    if (config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["eval_sequence"]]){
      
      # check if atleast one phase exists in the blocks section 
      if (length(names(config[["blocks"]])) == 0) { stop("blocks section in config file has no phase/block information") }
      
      # check if there is atleast one ttl_code in the blocks section
      if (sum(grepl("phys.ttl_code", names(unlist(config[["blocks"]], recursive = TRUE)))) == 0){ stop("block section in config file does not have any phys$ttl_code") }

      # check if each phase has an ntrials value associated with it
      unq_phases <- names(config[["blocks"]])
      if (sum(sapply(c(1:length(unq_phases)), function(x){is.null(config[["blocks"]][[unq_phases[x]]][["ntrials"]])})) > 0) { stop("some phases has no ntrials attribute in the blocks section of the config file") }
      
      # build the expected ttl code sequence
      expected_seq <- c()
      
      # add task start code the the sequence of expected ttl_codes
      if (!is.null(config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["task_start"]])){
        expected_seq <- c(expected_seq, config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["task_start"]])
      }
      
      for (p in c(1:length(unq_phases))){ # loop over the phases in the block section
        # get the list of events in this phase
        events_in_phase <- names(config[["blocks"]][[unq_phases[p]]][["events"]])
        
        # NOTE Assuming that one phase has one ntrials for all the events' ttl_codes which go in sequence with events mentioned in blocks
        expected_freq <- as.numeric(config[["blocks"]][[unq_phases[p]]][["ntrials"]])
        
        # sequence of ttl_codes inside a phase for one iteration through a block of that phase
        ttl_phase_seq <- rep(0, length(events_in_phase))
        sapply(c(1:length(events_in_phase)), function(x){ ttl_phase_seq[x] <- config[["blocks"]][[unq_phases[p]]][["events"]][[events_in_phase[x]]][["phys"]][["ttl_code"]] })

        # add sequence of ttl_code for this phase repeated for the expected frequency
        expected_seq <- c(expected_seq, rep(ttl_phase_seq, expected_freq))
      }
      
      # add task end code the the sequence of expected ttl_codes
      if (!is.null(config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["task_end"]])){
        expected_seq <- c(expected_seq, config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["task_end"]])
      }
      
      config[["definitions"]][["physio"]][["initialize"]][["ttl_codes_task"]][["expected_seq"]] <- expected_seq
    }
  }
  return(config)
}

