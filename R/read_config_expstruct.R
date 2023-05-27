#' @title create a table of expected experiment structure with rows for each event and columns for various event properties
#' 
#' @param config_path path to the YAML file containing configuration for running experiment.pipeline for a dataset, along with the experiment design structure 
#' @param behav dataframe of the behavioral datafile with rows for each event for all trials and blocks and columns containing the data ex: RTs, trial conditions, responses, etc.. 
#'     One column needs to be reference column with eventid to match the config and behav file rows. eventid is the column in exptd_trial that matches with the behav file column mentioned in event_col.
#'     
#' @return exptd dataframe of rows for each event in all blocks and phases of the experiment with their expected values in multiple properties in different datastream 
#' 
#' @importFrom dplyr %>% mutate row_number slice distinct slice pull filter left_join arrange select bind_rows
#' @importFrom tibble add_row add_column tibble
#' @importFrom stringr str_split
#' @importFrom yaml read_yaml
#' 
#' @author Nidhi Desai
#' 
## ---- run for development purpose ----
# filepath <- "C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.physio_dev"
# setwd("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Desktop/exp.physio_dev_temp_parked_here_temp_18th May 2023")
# invisible(lapply(c("read_config_supplementary.R", "read_config_expstruct_helpers.R"), source))
# source(file.path(filepath, "R/validate_exp_yaml.R"))
# pacman::p_load(yaml, checkmate, tidyverse, stringr)
# config_path <- file.path(filepath, "inst/extdata/ep_configs/weather/weather_all_modality_new_structure.yaml")
# # config <- validate_exp_yaml(file.path(filepath, "inst/extdata/ep_configs/weather/weather_all_modality_new_structure.yaml"))
# #if (if_null(expstruct$phases[[phase]]$event_seq) == "match_behav"){
#   behav_file_path <- file.path(filepath, "inst/extdata/raw_data/weather/255_cc_weather_behav.iqdat")
#   behav <- read.table(behav_file_path, sep="\t", header=TRUE) # iqdat are tab seperated files
# #}
# event_col <- "trialcode" # TODO later add code to pull this from config file


read_config_expstruct <- function(config_path, behav = NULL){
  
  # expected experimental data table
  empty_df <- data.frame(subID = numeric(),
                         row_num = numeric(),
                         event_type = character(),
                         eventid = character(),
                         trialid = character(),
                         trial_num = numeric(),
                         blockid = character(),
                         phaseid = character(),
                         event_dur = numeric(),
                         ntrials = numeric(), # in a block how many number of times will an event occur
                         ttl_code = numeric(),
                         eye_msg = character(),
                         behav_eventid = character(),
                         behav_find_rows = character(),
                         behav_nrow = numeric())
  exptd <- empty_df
  
  # read in the config file
  config <- validate_exp_yaml(config_path)
  expstruct <- config$exp_structure
  
  # extract the behav variables shorthand letter conversion list
  behav_subset_var <- lapply(config$definitions$behav$behav_subset_var, function(s){ trimws(str_split(s, "=")[[1]][2]) }) # behav subset variables and values list
  behav_subset_var <- setNames(behav_subset_var, unlist(lapply(config$definitions$behav$behav_subset_var, function(s){ trimws(str_split(s, "=")[[1]][1]) })))
  
  # readin behav file if we event_seq is match_behav
  if (expstruct$phases[[phase]][["event_seq"]] == "match_behav"){
    # TODO readin the behav data (add a function for this)
  }
  
  # ---------------------------------------------
  #      STEP 1. build out trials and blocks 
  # ---------------------------------------------
  # NOTES:
  # if event_seq is FALSE for a phase
  # events in a trial should be arranged in the order that is mentioned in the config (not like it will mater).
  # we might still be able to check the frequency of each trial's event or an event option. Add ntrials in the config file to the expected_trials. If there are options inside an event and the ntrials is present outside the event, that means that the total number of events should be ntrials irrespective of the options inside.
  
  # ASSUMPTION:
  # block is a repetition of the same trials without any change. A trial contains a set of events in a particular sequence.
  
  
  # create expected table for each block in all phases
  exp_level <- expstruct$exp
  phase_names <- names(exp_level$phases)
  exptd_blocks <- list() # list of dataframes each of which contain a events for all trials in one block
  for (p in seq_along(phase_names)){ 
    phase <- phase_names[p]
    block_names <- names(expstruct$phases[[phase]][["blocks"]])
    
    for (b in seq_along(block_names)){
      block <- block_names[b]
      nblocks <- ifelse(is.null(names(expstruct$phases[[phase]]$blocks[[block]])), 1,
                        ifelse(!("nblocks" %in% names(expstruct$phases[[phase]]$blocks[[block]])), 1,
                               expstruct$phases[[phase]]$blocks[[block]]$nblocks))
      block_level <- expstruct$blocks[[block]]
      block_level_ntrials <- if_null(block_level$ntrials)
      block_level_subsetvals <- if_null(block_level$block_seq_val) # all trials and events inside this block can be found by using these behav column values 
      
      trials_block <- block_level[["trials"]] # trials in the current block
      # trial_names <- unique(extract_list_levels(expstruct$trials, 1, FALSE))
      trial_names <- names(trials_block) # names of trials in this block
      
      
      # ==== create rows for each event in this block ==== 
      # --------------------------------------------------
      exptd_events <- list() # list of different trials containing rows for all events
      for (t in seq_along(trial_names)){
        trial <- trial_names[t]
        invisible(lapply(c("read_config_supplementary.R", "read_config_expstruct_helpers.R"), source))
        exptd_events[[trial]] <- create_exptd_trial(empty_df, trial, block, expstruct) # this function contains a call to the create_exptd_event() function
        # TODO figure out how to manage the "60 total" event_types
      }
      
      
      # ==== add all events and trials in this block in correct sequence ==== 
      # ---------------------------------------------------------------------
    
      # using all the exptd_events created for a particular block, join these rows of events to form a table for the entire block putting things inside a trial in sequence
      # both, arranging events inside a trial and arranging trials in a block are done below
      # loop going over trial_names given that we are building the event & trial sequence inside a block
      # TODO Think of situations where one block will have multiple trials which need to be stitched together, if this case arrives we need to add rows from the next trial to already created block_trials and not have this (block_trials <- exptd_trial) every trial_names
      
      # add this block's events to the expected blocks table
      invisible(lapply(c("read_config_supplementary.R", "read_config_expstruct_helpers.R"), source))
      exptd_blocks[[block]] <- create_exptd_block(empty_df, exptd_events, behav, event_col, 
                                                  nblocks, block, phase, expstruct)

      # exptd_blocks[[block]] <- block_trials
    }
  }
  
  
  # -----------------------------------------------------
  #      STEP 2. create expected table for all phases    
  # -----------------------------------------------------
  
  # add blocks in correct sequence to create a phase
  # combine multiple blocks into one phase 
  
  exptd_phases <- list() # list of dataframes each of which contains rows for events for all trials and all blocks in a phase
  for (p in seq_along(phase_names)){
    
    phase <- phase_names[p]
    block_names <- names(expstruct$phases[[phase]][["blocks"]])
    block_seq_vars <- behav_subset_var[expstruct$phases[[phase]]$block_seq_var] # there will be no block_seq_var if block_seq != "match_behav"
    
    # ==== find correct sequence of blocks ==== 
    # -----------------------------------------
    
    ## calculate total number of blocks in a phase
    nblocks_list <- lapply(block_names, function(x){
      ifelse("nblocks" %in% if_null(names(expstruct$phases[[phase]][["blocks"]][[x]])), 
             expstruct$phases[[phase]][["blocks"]][[x]][["nblocks"]], 1)})
    names(nblocks_list) <- block_names
    
    if (expstruct$phases[[phase]]$block_seq == "match_behav"){ # block sequence will be determined using the behav file
      block_seq_df <- get_block_seq_match_behav(nblocks_list, phase, behav_subset_var, expstruct, behav)
    } else { # if "match_config" is mentioned OR nothing is mentioned, "match_config" is assumed
      block_seq_df <- get_block_seq_match_config(nblocks_list)
    }
    
    # ==== get phase begin and end info ==== 
    # --------------------------------------
    
    # physio ttl code and eye messages sent at the phase begin and end
    phase_level <- expstruct$phases[[phase]]
    start_end_rows <- empty_df
    for (j in c("begin", "end")){
      if ("phys" %in% names(phase_level)){
        if (any(grepl(j, names(phase_level[["phys"]])))){
          start_end_rows <- start_end_rows %>% add_row(tibble(phaseid = phase, trialid = j, 
                                                              eventid = paste0(phase, "_", toString(j)), 
                                                              event_type = paste0("phase_", toString(j)),
                                                              ttl_code = phase_level$phys[[paste0("phase_", j, "_ttl")]]))
        }
      }
      if ("eye" %in% names(phase_level)){
        if (any(grepl(j, names(phase_level[["eye"]])))){
          start_end_rows <- start_end_rows %>% add_row(tibble(phaseid = phase, trialid = j, 
                                                              eventid = paste0(phase, "_", toString(j)), 
                                                              event_type = paste0("phase_", toString(j)),
                                                              eye_msg = phase_level$eye[[paste0("phase_", j, "_msg")]]))
        }
      }
    }
    start_end_rows <- start_end_rows %>% mutate(eye_msg = as.list(eye_msg),
                                                behav_find_rows = as.list(behav_find_rows))
    
    # ==== create expected table for phase ==== 
    # -----------------------------------------
    
    # add event rows for all blocks together in correct sequence
    all_blocks <- empty_df
    all_blocks <- all_blocks %>% mutate(eye_msg = as.list(eye_msg),
                                        behav_find_rows = as.list(behav_find_rows))
    
    # add phase begin row to exptd dataframe
    if ("begin" %in% start_end_rows$trialid){
      all_blocks <- all_blocks %>% add_row(start_end_rows[start_end_rows$trialid == "begin",])
    }
    
    # add the blocks' events in the existing exptd dataframe
    for (w in 1:nrow(block_seq_df)){
      all_blocks <- rbind(all_blocks, exptd_blocks[[block_seq_df$block_name[block_seq_df$block_num == w]]])
    }
    
    # add phase end row to exptd dataframe
    if ("end" %in% start_end_rows$trialid){
      all_blocks <- all_blocks %>% add_row(start_end_rows[start_end_rows$trialid == "end",])
    }
    
    exptd_phases[[phase]] <- all_blocks
  }
  
  
  
  # -------------------------------------------------------
  #      STEP 3. create expected table for entire task    
  # -------------------------------------------------------
  
  # add phases in correct sequence to create the entire task
  exptd <- empty_df
  exptd <- exptd %>% mutate(eye_msg = as.list(eye_msg),
                            behav_find_rows = as.list(behav_find_rows))
  
  # add experiment begin to exptd dataframe
  if ("exp_begin" %in% names(exp_level)){
    exptd <- exptd %>% add_row(eventid = "exp_begin",
                               ttl_code = ifelse(is.null(exp_level$exp_begin$ttl_code), NA, exp_level$exp_begin$ttl_code),
                               eye_msg = as.list(ifelse(is.null(exp_level$exp_begin$eye_msg), NA, exp_level$exp_begin$eye_msg)))
  }
  
  # add all phases rows in correct sequence begin to exptd dataframe
  # TODO add the case when someone mentions something other than "default" in front of the phase names in exp.
  for (p in seq_along(phase_names)){
    exptd <- rbind(exptd, exptd_phases[[phase_names[p]]])
  }
  
  # add experiment end to exptd dataframe
  if ("exp_end" %in% names(exp_level)){
    exptd <- exptd %>% add_row(eventid = "exp_end",
                               ttl_code = ifelse(is.null(exp_level$exp_end$ttl_code), NA, exp_level$exp_end$ttl_code),
                               eye_msg = as.list(ifelse(is.null(exp_level$exp_end$eye_msg), NA, exp_level$exp_end$eye_msg)))
  }
  
  return(exptd)
}
