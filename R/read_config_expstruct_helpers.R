#################################
##### List of subsidiary functions utilized in `read_config_expstruct()`
#################################
# - create_exptd_block()
#   -- event_seq_match_behav()
#   -- event_freq_match_config()
# - create_exptd_trial()
#   -- create_exptd_event()
#   -- get_ntrials_config()
# - get_block_seq_match_behav()
# - get_block_seq_match_config()

### other helper functions ####
# - extract_subset_vals()
# - if_null()
# - get_last_element()
# - get_row_num()
# - extract_list_levels()
# - add_values()
#################################

#' @title create a dataframe of expected properties of all events in trials in a block with correct expected sequence of trials and events
#'
#' @param empty_df a dataframe with no rows and all expected columns, used to add missing columns to dataframe created
#' @param exptd_events dataframe of expected event properties for all events in this block. Contains output from create_exptd_trial() function
#' @param behav dataframe of the behavioural datafile with rows for each event for all trials and blocks and columns containing the data ex: RTs, trial conditions, responses, etc.. 
#'     One column needs to be reference column with eventid to match the config and behav file rows. eventid is the column in exptd_trial that matches with the behav file column mentioned in event_col.
#' @param event_col is the column name in the behave file which contains event names for each row of data. This will be matched with the eventid column in exptd_trial
#' @param nblocks number of repetitions of the current block, so that the events and trials for only one repetition is present in the output. Used in event_seq_match_behav() function
#' @param block_f name of the block that the event belongs to
#' @param expstruct_f the experiment structure defined in the config file
#'
#' @return block_trials dataframe of expected event properties for all events in all trials in the current block
#'
#' @importFrom tidyverse %>% mutate row_number slice add_row distinct
#' 
#' @author Nidhi Desai
#' 
create_exptd_block <- function(empty_df, exptd_events, behav, event_col, nblocks, 
                               block_f, phase_f, expstruct_f){
  
  block_level <- expstruct_f$blocks[[block_f]]
  trials_block <- block_level[["trials"]] # trials in the current block
  trial_names <- names(trials_block) # names of trials in this block
  block_level_subsetvals <- if_null(block_level$block_seq_val) # all trials and events inside this block can be found by using these behav column values 
  
  # ==== add all events and trials in this block in correct sequence ==== 
  # ---------------------------------------------------------------------
  
  block_trials <- empty_df
  for (t in seq_along(trial_names)){ 
    trial <- trial_names[t]
    exptd_trial <- exptd_events[[trial]]
    
    # ---- determining event sequences ---- 
    # -------------------------------------
    # 1. event_seq = FALSE OR event_seq = "match_config" and event_freq = match_config for a block ----
    
    # CASE 1: 
    # event_seq = FALSE and event_freq = match_config
    # events do not need to be in correct sequence so adding events with option in sequence mentioned in the config file. The sequence of these won't be checked in quality checks
    # but the frequency of events in a block need to be correct, so for options where the ntrials in not mentioned, some random sequence cannot be added to the expected table.
    # here even for this case we are creating an expected table with events in sequence as mentioned in config file and options in the events. This will not be used for testing data during quality checks for this case. 
    # CASE 2:
    # event_seq = "match_config" and event_freq = match_config
    # example:- the Pav phase can have event_seq=match_config and Pit phase event_seq=FALSE
    # if event_seq=match_config this would mean that the event_type sequences will be matched but not the options within the events. 
    # if there is a pattern to the sequence of options, this indicates a need for separate trials for each type of option for this event_type and these trials can be added in correct sequence in the block.
    
    if ((if_null(expstruct_f$phases[[phase_f]]$event_seq) == FALSE | if_null(expstruct_f$phases[[phase_f]]$event_seq) == "match_config") & 
        if_null(expstruct_f$phases[[phase_f]]$event_freq) == "match_config"){ 
      block_trials <- event_freq_match_config(exptd_trial)
      block_trials <- block_trials %>% mutate(row_num = row_number()) %>%
        mutate(trial_num = behav[block_trials$behav_nrow, expstruct_f$phases[[phase_f]][["ntrial_var"]]]) # add trial numbers from the behav file 
      block_trials[, setdiff(names(empty_df), names(block_trials))] <- NA # add missing columns as NA to match the exptd dataframe
    }
    
    
    # ---- 2. event_seq = match_behav and event_freq = match_behav for a block ----
    # When event_seq is set to match_behav, the assumption is that the behav file contains the accurate sequence of events 
    # and other datastreams (physio and eye) should be matched to the sequence in behav data.
    # Here instead of an event containing all possible options in that row in the expected data like in the 1st case mentioned above, the option which appears in the behav file will be placed in the expected data.
    # Here block trials will be created by the sequence in actual behav data and matching our expected event to the
    # rows in the behav data and kindof replacing our expected event into the sequence we already see in the behav file
    
    else if (if_null(expstruct_f$phases[[phase_f]]$event_seq) == "match_behav" &
             if_null(expstruct_f$phases[[phase_f]]$event_freq) == "match_behav"){
      block_trials <- event_seq_match_behav(exptd_trial, behav, event_col, nblocks)
      block_trials <- block_trials %>% mutate(row_num = row_number()) %>% 
        mutate(trial_num = behav[block_trials$behav_nrow, expstruct_f$phases[[phase_f]][["ntrial_var"]]]) # add trial numbers from the behav file 
      block_trials[, setdiff(names(empty_df), names(block_trials))] <- NA # add missing columns as NA to match the exptd dataframe
    }
    
    # TODO Check what happens when there is a repeat trial and 2 consecutive display_both_stimuli appear in block_trials
    
    
    # ---- 3. event_seq = match_behav and event_freq = match_config for a block ----
    
    # When event_seq is set to match_behav, the assumption is that the behav file contains the accurate sequence of events 
    # and other datastreams (physio and eye) should be matched to the sequence in behav data.
    # Even though the sequence is determined by the behav file here, event_freq can still be set to match_config.
    # This would mean that the behav file and other datastreams's event freq will be checked against the config file.
    # NOTE: the count through sequence and freq from config might be different??
    # Here instead of an event containing all possible options in that row in the expected data, the option which appears in the behav file will be placed in the expected data.
    # Also instead of assuming that the config file contains the events in correct sequence
    
    # TODO the sequence is built based on the behav file, but in order to make sure that the frequencies are correct, the frequency will be added into the expected structure 
    # how to we incorporate freq into the event sequence already formed using the sequence from behav file??
    
    # the config file would still contain the correct experiment structure because the event_freq options contains "match_config" 
    # which indicates that the config file will be used for frequency matching. So the expectation that the config will contain information
    # about the frequency of each event is acceptable.
    
    else if (if_null(expstruct_f$phases[[phase_f]]$event_seq) == "match_behav" &
             if_null(expstruct_f$phases[[phase_f]]$event_freq) == "match_config"){
      # NOTE block_trials is the table created from the behav data and block_trials_config is the table created from the config file.
      
      # block_trials_config table created from only the config file, reference to add events in the block_trials created from the behav data so that the freq of events could be matched to the config file
      block_trials_config <- event_freq_match_config(exptd_trial) 
      block_trials_config <- block_trials_config %>% filter(event_type != "repeatPitTrial")
      
      # block_trials table below is created from the behav file and the events are in correct sequence as per the behav data. 
      block_trials <- event_seq_match_behav(exptd_trial, behav, event_col, nblocks)
      block_trials <- block_trials %>% mutate(row_num = row_number()) %>% 
        mutate(trial_num = behav[block_trials$behav_nrow, expstruct_f$phases[[phase_f]][["ntrial_var"]]]) # add trial numbers from the behav file 
      block_trials[, setdiff(names(empty_df), names(block_trials))] <- NA # add missing columns as NA to match the exptd dataframe
      
      ## incorporating frequencies and sequence together correctly by merging block_trials_config and block_trials
      
      # create one trial first from the config file and find the sequence of events in this trial from the behav
      # then repeat the trial in the block as per the frequencies in config file
      # then replace some of the events with options with the specific options from the behav file
      # TODO this won't work for cases when inside a block the structure of trials change. Will there be any such cases?
      # basically the sequence from behav file is used for 2 purposes: 
      # 1. sequence of events inside a trial
      # 2. For events with options, knowing which option occurred in each of the trials
      
      # correct the sequence of events inside a trial in the block_trials_config using block_trials
      exptd_trial <- exptd_trial %>% filter(event_type != "repeatPitTrial") # TODO later remove this after we figure out how to deal with the extra option in trial flow
      trial_evs <- block_trials_config[1:length(unique(exptd_trial$event_type)),] # these events constitute a trial and is trial is the set of events that are repeated to create block_trials
      ev_seq_behav <- unlist(lapply(trial_evs$event_type, function(x){ which(block_trials$event_type == x)[1] }))
      trial_evs <- trial_evs %>% arrange(ev_seq_behav)
      
      # calculate number of trials in this block
      evs_with_options <- unique(exptd_trial[duplicated(exptd_trial$event_type), "event_type"])
      common_ntrials <- unique(exptd_trial$ntrials[!exptd_trial$event_type %in% evs_with_options]) # TODO might not work for all situations
      common_ntrials <- as.numeric(common_ntrials[!is.na(common_ntrials)])
      copy_rows <- rep(1:nrow(trial_evs), common_ntrials)
      trial_num <- rep(1:common_ntrials, each = nrow(trial_evs))
      block_trials_config <- trial_evs %>% slice(copy_rows) %>% mutate(trial_num = trial_num)
      block_trials_config <- block_trials_config %>% mutate(row_num = row_number())
      behav <- behav %>% mutate(row_number = row_number())
      
      # add/delete rows of events from the block_trials which does not match block_trials_config, so that the events with options can be correctly matched across the two tables
      # compare frequencies of events in both expected tables from behav and config file
      f_ref <- table(block_trials_config$event_type)
      f_compare <- table(block_trials$event_type)
      all_elems <- union(names(f_ref), names(f_compare))
      miss_ev <- all_elems[f_ref[all_elems] > f_compare[all_elems]]
      extra_ev <- all_elems[f_ref[all_elems] < f_compare[all_elems]]
      
      # adding missing events to block_trials
      if (length(miss_ev) > 0) { # length(miss_ev) is 0 means no mismatch in frequencies for any events
        
        # convert block_trials eventid from character to list
        block_trials$eventid <- lapply(block_trials$eventid, function(x){c(x)})
        block_trials$event_dur <- lapply(block_trials$event_dur, function(x){c(x)})
        block_trials$ttl_code <- lapply(block_trials$ttl_code, function(x){c(x)})
        block_trials$behav_eventid <- lapply(block_trials$behav_eventid, function(x){c(x)})
        
        # get the trial numbers where events are missing from the trial counter column in the behav file
        trial_num_behav <- behav %>% filter(row_number %in% block_trials$behav_nrow) %>% pull(pitNumTrialsCounter)
        trial_num_expected <- nrow(exptd_trial %>% distinct(event_type, .keep_all = TRUE) %>% distinct(behav_eventid, .keep_all = TRUE))
        missing_trial_num <- unname(which(table(trial_num_behav) != trial_num_expected))
        # get the trial numbers where the event might be a separate event (part of the present event) in behav file but is still missing and can be found by the frequency of trial numbers in block_trials
        missing_trial_num <- unique(c(missing_trial_num, unname(which(table(block_trials$trial_num) != nrow(trial_evs)))))
        
        # add the missing event rows into block_trials from the block_trials_config table
        for (m in missing_trial_num){
          # add missing row to block_trials
          present_ev_trial <- block_trials %>% filter(trial_num == m) %>% pull(event_type)
          present <- trial_evs$event_type %in% present_ev_trial
          present_rownum <- block_trials %>% filter(trial_num == m) %>% pull(row_num)
          exptd_trial_rownum <- rep(NA, nrow(trial_evs))
          exptd_trial_rownum[present] <- present_rownum
          
          # populate missing event's expected row numbers
          curr_trial_rownum <- ((m-1)*nrow(trial_evs)+1): (m*nrow(trial_evs))
          missing_rownum <- curr_trial_rownum[!present]
          
          for (me in missing_rownum){
            temp_row <- trial_evs[curr_trial_rownum == me,]
            temp_row <- temp_row %>% add_column(behav_nrow = NA) %>% add_column(trial_num = m) %>% mutate(row_num = me)
            block_trials <- block_trials %>% add_row(temp_row, .after = me-1)
          }
          block_trials <- block_trials %>% mutate(row_num = row_number())
        }
      }
      
      
      # removing extra events to block_trials
      # TODO do we really need this, what the chance of this possibility?
      if (length(extra_ev) > 0) {
        # each trial should have one count of each event, more than that would suggest additional unwanted events
        
        # get the trial numbers where extra events are present compared to expected exptd_trial table
        trial_num_expected <- nrow(exptd_trial %>% distinct(event_type, .keep_all = TRUE))
        extra_trial_num <- unname(which(table(block_trials$trial_num) != trial_num_expected))
        
        # remove the extra rows from block_trials
        all_evs <- trial_evs$event_type
        for (m in extra_trial_num){
          block_trials <- block_trials %>% mutate(row_num = row_number())
          this_trial_evs <- block_trials %>% filter(trial_num == m) 
          
          # There are there ways to have extra rows --
          
          if (nrow(this_trial_evs) > trial_num_expected){
            # do all events exists in this trial?
            if (!all(all_evs %in% this_trial_evs$event_type)) {
              stop(paste("trial number", m, "dont contain all events prior to checking extra events."))
            }
            
            # 1. delete rows whose event is not in the all_evs list
            indx <- which(!(this_trial_evs$event_type %in% all_evs))
            if (length(indx) > 0) {
              block_trials <- block_trials %>% slice(-this_trial_evs$row_num[indx])
            }
            
            # 2. extra events in existing trials
            
            # the first occurrence of the event will be considered as the correct one and repeatitions of the event will be removed from block_trials
            delete_rows <- c()
            duplicated_ev <- this_trial_evs$event_type[duplicated(this_trial_evs$event_type)]
            nondup_rows <- which(!(this_trial_evs$event_type %in% duplicated_ev))
            for (i in duplicated_ev){
              relative_position <- which(all_evs == i)
              this_trial_evs$event_type[relative_position]
              # TODO this needs to be figured out
              
            }
          }
          
          # 3. extra trials with all or some events
          if (m > common_ntrials){
            block_trials <- block_trials %>% slice(-this_trial_evs$row_num)
          }
        }
      }
    }
    
    # ---- 4. event_seq = FALSE and event_freq = FALSE for a block----
    # TODO figure out if we need this option later
  }
  
  
  # ==== add block begin and end information to block_trials ==== 
  # -------------------------------------------------------------
  
  block_begin_end <- empty_df
  block_begin_end <- block_begin_end %>% mutate(eye_msg = as.list(eye_msg))
  for(i in c("block_begin", "block_end")){
    if (i %in% names(block_level)){
      block_begin_end <- block_begin_end %>% add_row(eventid = i, event_type = i, trialid = trimws(sub("block_", "", i)),
                                                     ttl_code = ifelse(is.null(block_level[[i]][["ttl_code"]]), NA, block_level[[i]][["ttl_code"]]),
                                                     eye_msg = I(ifelse(is.null(block_level[[i]][["eye_msg"]]), as.list(NA), as.list(block_level[[i]][["eye_msg"]]))))
    }
  }
  
  if(!is.null(block_level_subsetvals) & (nrow(block_begin_end) > 0)){ # add block level behav_find_rows common to all evnets in a block
    subset_ls <- extract_subset_vals(behav_subset_var, block_level_subsetvals)
    block_begin_end <- block_begin_end %>% 
      mutate(behav_find_rows = list(subset_ls))
  }
  
  # add block begin and end trials to block_trials
  block_begin_end <- block_begin_end %>% 
    mutate(eye_msg = as.list(eye_msg)) %>%
    mutate(ntrials = as.character(ntrials))
    
  # add the block level behav_find_rows if not present already
  if(nrow(block_begin_end) > 0){
    block_trials <- block_trials %>% add_row(block_begin_end[1,], .before = 1)
  }
  if (nrow(block_begin_end) == 2){
    block_trials <- block_trials %>% add_row(block_begin_end[2,], .after = nrow(block_trials))
  }
  
  return(block_trials)
}



#' @title create a dataframe of expected properties of all events in a trial
#' 
#' @param empty_df a dataframe with no rows and all expected columns, used to add missing columns to dataframe created
#' @param trial_f name of the trial that the event belongs to
#' @param block_f name of the block that the event belongs to
#' @param expstruct_f the experiment structure defined in the config file
#'
#' @return new_trial dataframe of expected event properties in the current trial
#' 
#' @author Nidhi Desai
#' 
create_exptd_trial <- function(empty_df, trial_f, block_f, expstruct_f){

  # ---- trial level information ----
  # ---------------------------------
  
  new_trial <- empty_df # dataframe for this trial type
  
  # find which phase this block_f belongs too
  phase_extract <- extract_list_levels(expstruct_f$phases, 3, full.names = TRUE)
  phase_extract <- phase_extract[grepl(block_f, phase_extract)]
  phase_f <- strsplit(phase_extract, ".", fixed = TRUE)[[1]][1]
  
  block_level <- expstruct_f$blocks[[block_f]]
  trials_block <- block_level[["trials"]] # trials in the current block
  trial_level <- expstruct_f$trials[[trial_f]]
  
  ## some trials used in a block might have some part of the trial changed, this needs to be added while building the trial
  if (is.list(trials_block[[trial_f]])){ # not default trial definition
    events_to_change <- extract_list_levels(trials_block[[trial_f]], 1, FALSE)
    for (q in events_to_change){
      prop_to_change <- names(trials_block[[trial_f]][[q]])
      for (p in prop_to_change){ # properties of an event to change
        trial_level[[q]][[p]] <- trials_block[[trial_f]][[q]][[p]]
      }
    }
  }
  trial_events <- unique(extract_list_levels(trial_level, 1))
  
  ## Options directly under trials (example trial_pit)
  # here the events under options are being extracted out and treated like a normal event, so that an event row can be added to the expected trial
  option_layers <- unlist(lapply(trial_events, function(x){grepl("option", x)}))
  if(any(option_layers)){ # if there are options at the event level for different options how a trial will flow
    for (y in trial_events[option_layers]){
      layers_pullout <- unique(extract_list_levels(trial_level[[y]],1))
      layers_pullout <- layers_pullout[nchar(layers_pullout) > 0]
      for (z in layers_pullout){
        trial_level[[z]] <- trial_level[[y]][[z]]
      }
      trial_level[[y]] <- NULL
    }
    trial_events <- unique(extract_list_levels(trial_level, 1))
  }
  
  # ---- build rows for all events in this trial ----
  # -------------------------------------------------
  
  for (e in seq_along(trial_events)){ # loop over events in this trial
    ev <- trial_events[e]
      
    #### options in trials ####
    # if a trial_level contains "option", then we will create separate trial dataframes for each option
    # "option" present under events in trials is dealt with here
    # NOTE the following only takes care of options under events in a trial
    if (any(unlist(lapply(extract_list_levels(trial_level[[ev]], 1, TRUE), function(x){grepl("option", x)})))){
      unlisted_trial_level <- unlist(trial_level[[ev]])
      # test if atleast 2 options are mentioned at the same level
      temp <- unique(extract_list_levels(trial_level[[ev]], 1, TRUE))
      num_option <- length(temp[nchar(temp) > 0])
      if (num_option < 2) {stop(paste0("only one option is mentioned in trial", trial_f,  " and event ", ev,". Atleast two options are required."))}
      ev_id <- unlist(lapply(c(1:num_option), function(x) { unlisted_trial_level[[paste0("option", toString(x), ".def")]] }))
    } else {
      ev_id <- trial_level[[ev]][["def"]]
    }
    
    ## extract the number of expected trials containing the event
    ev_opt_ntrials <- get_ntrials_config(ev, length(ev_id), trial_f, block_f, expstruct_f)
    
    #### create a row of data for this event ####
    # ev_id has multiple elements if there are options inside an event
    for (k in seq_along(ev_id)){
      ev_opt <- ev_id[[k]]
      new_event <- create_exptd_event(ev, ev_opt, ev_id, expstruct_f, trial_f, block_f, phase_f)
      
      # add rows for each event to the trial dataframe
      new_trial <- rbind(new_trial, new_event) 
    }
  }
  return(new_trial)
}



#' @title creates expected event data for a given event option in a trial
#'
#' @description it extracts event properties from the experiment structure and builds a new row in the expected data table for the current event in the specific trial and block.
#'
#' @param ev the event type for which we are creating the expected table
#' @param ev_opt the specific event option for that we are creating the expected table, in case there are multiple options available for the event mentioned in the config file
#' @param ev_id list of options for the current event, as mentioned in the config file
#' @param expstruct_f the experiment structure defined in the config file
#' @param trial_f name of the trial that the event belongs to
#' @param block_f name of the block that the event belongs to
#' @param phase_f name of the phase that the event belongs to
#'
#' @return new_event returns a dataframe with one row for the current options of the event and columns with expected properties for this event
#'
#' @importFrom dplyr add_column mutate
#' @importFrom stringr str_split
#' 
#' @author Nidhi Desai
#'
create_exptd_event <- function(ev, ev_opt, ev_id, expstruct_f, 
                               trial_f, block_f, phase_f){
  ev_level <- expstruct_f$events[[ev_opt]]
  
  # ---- extract the event information from the config file ----
  # ------------------------------------------------------------
  
  temp <- unique(extract_list_levels(expstruct_f$trials[[trial_f]], 2, TRUE))
  ev_option_num <- str_split(temp[grepl(paste0(".", ev), temp)], "\\.")
  if(length(ev_option_num) == 0){ # no options in trial flow
    ev_struct <- expstruct_f$trials[[trial_f]][[ev]]
  } else if (length(ev_option_num) == 1){ # one flow option contains this event
    ev_struct <- expstruct_f$trials[[trial_f]][[ev_option_num[[1]][1]]][[ev]]
  } else {
    stop (paste("Many options in trial flow which contain the event:", ev))
  }
  
  # ---- add modified event definition ----
  # ---------------------------------------
  # some events used in a trial might have some part of the event changed, this needs to be added while building the event
  if(length(ev_id) > 1){
    event_level_info <- ev_struct[[paste0("option", toString(which(ev_id == ev_opt)))]] # if multiple options in this event
  } else {
    event_level_info <- ev_struct
  }
  
  if (length(names(event_level_info)) > 1){
    events_changed <- names(event_level_info)
    if(!"def" %in% events_changed) { stop(paste("trial", trial_f, "event", ev, "does not have a def (definition) i.e. name of a event.")) } # make sure one of these is "def" which gives the event name that needs to be added here
    events_changed <- events_changed[events_changed != "def"]
    if ("ntrials" %in% events_changed) { events_changed[events_changed != "ntrials"] }
    for (c in events_changed){# change the original event property or add a new property
      ev_level[[c]] <- event_level_info[[c]]
    }
  }
  
  # ---- extract the number of expected trials containing the event ----
  # --------------------------------------------------------------------
  ev_opt_ntrials <- get_ntrials_config(ev, length(ev_id), trial_f, block_f, expstruct_f)
  
  # ---- form a new row for the current event in this specific trial and block ----
  # -------------------------------------------------------------------------------
  new_event <- data.frame(event_type = ev, eventid = ev_opt, trialid = trial_f, blockid = block_f, phaseid = phase_f,
                          event_dur = if_null(ev_level$dur), ttl_code = if_null(ev_level$ttl_code), 
                          eye_msg = I(ifelse(is.null(ev_level$eye_mid_msg), NA, list(ev_level$eye_mid_msg))),
                          ntrials = ev_opt_ntrials[which(ev_id == ev_opt)])
  
  # ---- add behav file subset variables ----
  # -----------------------------------------
  # add behav file subset variables and values list to the expected datatable
  if ("behav" %in% names(config$definitions)){ # does behav data need to be processed?
    
    # the event definition contains eventid_val 
    new_event <- new_event %>% add_column(behav_eventid = ifelse(!is.null(ev_level$eventid_val), ev_level$eventid_val, NA))
    
    # corresponding event related data from the behav file can be found using either behav_subset_val mentioned for the entire block or behav_find_rows specifically for an event
    block_level_subsetvals <- if_null(expstruct_f$blocks[[block_f]]$block_seq_val) # all trials and events inside this block can be found by using these behav column values 
    
    if(!is.null(block_level_subsetvals)) { 
      subset_ls <- extract_subset_vals(behav_subset_var, block_level_subsetvals)
      new_event <- new_event %>% add_column(behav_find_rows = list(subset_ls))
    } else {
      new_event <- new_event %>% add_column(behav_find_rows = NA)
    }
    
    if (!is.null(ev_level$behav_subset_val)){
      subset_ls <- lapply(ev_level$behav_subset_val, function(s){ as.numeric(trimws(str_split(s, "=")[[1]][2])) }) 
      match_letters <- unlist(lapply(ev_level$behav_subset_val, function(s){ trimws(str_split(s, "=")[[1]][1]) }))
      subset_ls <- setNames(subset_ls, unlist(lapply(match_letters, function(x){ behav_subset_var[[x]] })))
      if(is.na(new_event$behav_find_rows)) {
        new_event <- new_event %>% add_column(behav_find_rows = list(subset_ls))
      } else {
        new_event <- new_event %>% mutate(behav_find_rows = list(append(unlist(behav_find_rows), subset_ls)))
      }
    }
    
    # TODO remove this line later and figure out how to deal with "60 total" ntrials
    # new_event$ntrials <- ifelse(grepl("total", as.character(new_event$ntrials)),
    #                             as.numeric(trimws(sub("total", "", new_event$ntrials))),
    #                             new_event$ntrials)
  }
  return(new_event)
}



#' @title extracts the number of expected trials especially when there are options within an event
#' @description if there are no options inside an event, the "ntrials" mentioned under the event is the expected number of trials.
#'     If the "ntrials" is present inside an option inside the event, then this function checks if optionwise ntrials are present inside an event.
#'     If not, then the ntrials which is mentioned in the event but outside the options is the total number of events expected irrespective of which
#'     option appears. The function returns this ntrials number with the added text " total".
#' @param ev name of the event type
#' @param len_ev_opts length of the ev_id list variable which contains all possible options for the ev event type
#' @param trial_f name of the current trial
#' @param block_f name of the current block
#' @param expstruct_f the experiment structure defined in the config file
#' 
#' @return ev_opt_ntrials a list (of length of number of options or) a number of expected number of trials of an event from the config file
#' 
#' @importFrom stringr str_split
#' 
#' @author Nidhi Desai
#' 
get_ntrials_config <- function(ev, len_ev_opts, trial_f, block_f, expstruct_f){
  
  block_level_ntrials <- if_null(expstruct_f$blocks[[block_f]]$ntrials)
  
  # ---- extract ntrials for options in a trial flow ----
  # -----------------------------------------------------
  
  ## extact ntrials for options in a trial (different possible trial paths), if present
  trial_2nd_level <- extract_list_levels(expstruct_f$trials[[trial_f]], 2, full.names = TRUE)
  options_ntrials <- grepl("option", trial_2nd_level) &  grepl("ntrials", trial_2nd_level)
  if (sum(options_ntrials) >= 1){ # we have trial options and ntrials
    trial_2nd_level <- trial_2nd_level[options_ntrials]
    options_ntrials <- as.numeric(expstruct_f$trials[[trial_f]][[sub(".ntrials", "", trial_2nd_level)]]$ntrials)
    block_level_ntrials <- options_ntrials
    trial_level <- expstruct_f$trials[[trial_f]][[sub(".ntrials", "", trial_2nd_level)]][[ev]]
  } else {
    trial_level <- expstruct_f$trials[[trial_f]][[ev]]
  }
  
  # ---- extract ntrials for no options inside an event ----
  # --------------------------------------------------------
  
  if(len_ev_opts == 1){ # no options inside an event
    if ("ntrials" %in% names(trial_level)){
      ev_opt_ntrials <- trial_level[["ntrials"]]
    } else if (!is.na(block_level_ntrials)) {
      ev_opt_ntrials <- block_level_ntrials
    }
  }
  
  
  # ---- extract ntrials for options inside an event ----
  # -----------------------------------------------------
  
  if (len_ev_opts > 1){ # options are present inside an event
    
    nt_in_event <- names(unlist(trial_level))[grepl("ntrials", names(unlist(trial_level)))]
    if (length(nt_in_event) > 0){
      if (any(grepl("option", nt_in_event))){ # if the "ntrials" is present inside an option inside the event
        if(length(nt_in_event) == len_ev_opts){ # optionwise ntrials are present inside an event
          ev_opt_ntrials <- sapply(1:len_ev_opts, function(x){ as.numeric(unlist(expstruct_f$trials[[trial_f]][[ev]])[[paste0("option", toString(x), ".ntrials")]]) })
          if(!is.na(block_level_ntrials) & (sum(ev_opt_ntrials) != block_level_ntrials)) { stop(paste("ntrials in options for trial", trial_f, " and event ", ev, "do not add upto the number of trials")) } # check if the sum of ntrials in option is equal to the trial level ntrials, if mentioned
        } else { # if the ntrials is outside any options but inside the event
          ev_opt_ntrials <- c(paste(toString(expstruct_f$trials[[trial_f]][[ev_option_num]][[ev]][["ntrials"]]), "total") , rep(NA, len_ev_opts-1))
        }
      }

    } else {
      if (sum(grepl("ntrials", names(unlist(expstruct_f$trials[[trial_f]][[ev]])))) == 0) { # no optionwise ntrials are present inside an event
        if (!is.na(block_level_ntrials)){
          ev_opt_ntrials <- c(paste(toString(block_level_ntrials), "total") , rep(NA, len_ev_opts-1))
        } else {
          # check if this event is under an option for trial flow which might contain the ntrials for this event
          temp <- unique(extract_list_levels(expstruct_f$trials[[trial_f]], 2, TRUE))
          ev_option_num <- str_split(temp[grepl(paste0(".", ev), temp)], "\\.")[[1]][1]
          if("ntrials" %in% names(expstruct_f$trials[[trial_f]][[ev_option_num]])) {
            ev_opt_ntrials <- c(paste(toString(expstruct_f$trials[[trial_f]][[ev_option_num]][["ntrials"]]), "total") , rep(NA, len_ev_opts-1))
          } else {
            ev_opt_ntrials <- c(paste(toString(block_level_ntrials), "total") , rep(NA, len_ev_opts-1))
          }
        }
      } else { # only some of all the options have ntrials
        stop(paste("number of trials are not mentioned in all options in event trial", trial_f, "event", ev, ". Either add ntrials to all or none of the options."))
      }
    }
  }
  return(ev_opt_ntrials)
  
}


#' @title build a dataframe of all expected events and trials in a block from the config file, when the event_freq for the phase is set to match_config
#' @param exptd_trial contains the exptd_events dataframe for a trial.
#' 
#' @return block_trials dataframe of all events and trials in a block created based on the config file
#' 
#' @importFrom dplyr %>% slice pull
#'
#' @author Nidhi Desai
#' 
event_freq_match_config <- function(exptd_trial){
  if (sum(duplicated(exptd_trial$event_type)) == 0){ # no options in an event
    # all events can be one after the other so repeat the set of trials ntrials number of times
    block_trials <- exptd_trial %>% slice(rep(1:n(), unique(exptd_trial$ntrials)))
    
  } else { # options in events
    # it doesn't matter here what is the sequence in which the options in an event re expected to apear
    # only that the frequency of these options should be accurate.
    # here we will be adding the 2 options in the sequence they were mentioned in the config file with the trials with one of the options consecutive to each other.
    # multi_rows <- which(duplicated(exptd_trial$event_type))
    
    # Identify the duplicated values and get their indexes
    dup_values <- unique(exptd_trial[duplicated(exptd_trial$event_type), "event_type"])
    dup_indexes <- lapply(dup_values, function(val) which(exptd_trial$event_type == val))
    
    # build a basic table for block trials, events in these will be changed based on options for the events
    block_trials <- exptd_trial[!duplicated(exptd_trial$event_type), ]
    common_ntrials <- unique(exptd_trial$ntrials[!exptd_trial$event_type %in% dup_values]) # TODO might not work for all situations
    common_ntrials <- as.numeric(common_ntrials[!is.na(common_ntrials)])
    if (length(common_ntrials) > 1) {stop("ntrials in non-option events are not same for all these events")}
    copy_rows <- rep(1:nrow(block_trials), common_ntrials)
    block_trials <- block_trials %>% slice(copy_rows)
    
    for (d in seq_along(dup_indexes)){ # in case of multiple event_types with options
      dup_rows <- dup_indexes[[d]]
      
      # # replace the event rows with the appropriate option for that event in block_trials   ----        
      # if (sum(is.na(exptd_trial$ntrials[dup_rows])) == 0){ # the ntrials column has expected number of trials for each option in an element, this is used to calculate frequency of each option.
      #   # get ntrials from a non-options event
      #   nt <- unique(exptd_trial %>% slice(-dup_rows) %>% pull(ntrials))
      #   if (length(nt) > 1) {stop (paste("ntrials for events without any option are different from each other. The ntrials need to be same across."))}
      #   
      #   # block_trials <- block_trials %>% slice(rep(1:n(), nt))
      #   
      #   dup_event_option_loc <- dup_rows[2:length(dup_rows)]
      #   dup_event_ntrials <- exptd_trial$ntrials[dup_rows]
      #   dup_event_loc <- dup_rows[1] # location inside a trial of this duplicate event
      #   
      #   rep_ev <- exptd_trial %>% slice(dup_rows) # replace event
      #   
      #   # rep_ev <- exptd_trial %>% slice(dup_rows[2:length(dup_rows)]) # rows for repeated events that will be used to replace the row in first location for this event
      #   # other_evs <- exptd_trial %>% slice(-dup_rows[2:length(dup_rows)]) # rows for repeated events
      #   # block_trials <- block_trials %>% rbind(do.call(rbind, replicate(dup_event_ntrials[1], other_evs, simplify = FALSE)))
      #   
      #   # here we will find the row for the current event and replace them as per the ntrials
      #   # this will work for multiple situations:
      #     # when the event is the first one among all events with options
      #     # when for a previous event, options have been added to block_trials
      #   counter <- 1
      #   for (m in c(1:nrow(rep_ev))){
      #     # other_evs[dup_event_loc,] <- rep_ev %>% slice(m)
      #     ev_rows <- which(block_trials$event_type == dup_values)
      #     indx <- counter:(counter + dup_event_ntrials[m] - 1)
      #     replace_rows <- ev_rows[indx]
      #     block_trials[replace_rows,] <- rep_ev %>% slice(m)
      #     counter <- length(replace_rows) + 1
      #     # block_trials <- block_trials %>% rbind(do.call(rbind, replicate(dup_event_ntrials[m], other_evs, simplify = FALSE)))
      #   }
      # } ----
      
      # if either the ntrials column has expected number of trials for each option OR
      # total ntrials for the event is present but not for individual options
      if (!(sum(is.na(exptd_trial$ntrials[dup_rows])) == 0 | grepl("total", exptd_trial$ntrials[dup_rows[1]]))){
        stop(paste("options for event", dup_values[d], "in trial", trial_f, "don't have number of trials information to use event_freq = TRUE.
                \n Either add ntrials to the options or set seq_freq = FALSE."))
      }
      
      if (sum(is.na(exptd_trial$ntrials[dup_rows])) == 0){ # the ntrials column has expected number of trials for each option in an element, this is used to calculate frequency of each option.
        # get ntrials from a non-options event
        nt <- unique(exptd_trial %>% slice(-dup_rows) %>% pull(ntrials))
      } else if (grepl("total", exptd_trial$ntrials[dup_rows[1]])){ # the ntrials for each options is not mentioned in the config file but the total ntrials for the event irrespective of which option appears in a particular trial,
        # is mentioned in the first row for the event in exptd_events as "<ntrials> total".
        # get ntrials from a non-options event
        # for (i in seq_along(exptd_trial$ntrials)){
        #   if(grepl("total",exptd_trial$ntrials[i])){
        #     exptd_trial$ntrials[i] <- as.numeric(trimws(sub("total", "", exptd_trial$ntrials[i])))
        #   }
        # }
        ntrials_event <- as.numeric(trimws(sub("total", "", exptd_trial$ntrials[dup_rows[1]])))
        nt <- exptd_trial %>% slice(-dup_rows) %>% pull(ntrials) # total ntrials for an event without options
        nt <- sapply(nt, function(x){ ifelse(grepl("total",x), as.numeric(trimws(sub("total", "", x))), as.numeric(x)) }, USE.NAMES = FALSE)
        nt <- nt[!is.na(nt)]
        nt <- unique(nt)
      }
      
      # check on unique value of ntrials for this trial
      if (length(nt) > 1) {stop (paste("ntrials for events without any option are different from each other. The ntrials need to be same across."))}
      
      # Since there are NAs in the ntrials, we can't know how many times to expect an option to appear for an event. 
      # Therefore, we will add each event's info into a list in one row for the event. The list will be useful to test the frequency match for the event and not the options in particular.
      # for example, the ttl-codes will be one of the multiple option's ttl-codes and their sum would need to match the total ntrials for the block.
      
      # combine all rows for all options for an event in the expected table
      rep_ev <- exptd_trial %>% slice(dup_rows[1])
      for (h in colnames(rep_ev)) {rep_ev[[h]] <- list(c(rep_ev[[h]]))}
      for (f in dup_rows[2:length(dup_rows)]){
        for (h in colnames(rep_ev)){
          val1 <- rep_ev[[h]][[1]]
          val2 <- exptd_trial[f, h]
          rep_ev[[h]] <- ifelse(all(is.na(val1) & is.na(val2)), NA, list(c(val1, val2)))
        }
      }
      rep_ev$ntrials <- ifelse(grepl("total", exptd_trial$ntrials[dup_rows[1]]), ntrials_event, rep_ev$ntrials)
      rep_ev$event_type <- unique(rep_ev$event_type[[1]])
      
      # only one value in this column for all options
      for (h in colnames(rep_ev)){
        if (length(unique(rep_ev[[h]][[1]])) == 1){ 
          rep_ev[[h]] <- unique(rep_ev[[h]][[1]])
        }
      }
      
      # here we will find the row for the event and replace them with the combined options row
      ev_rows <- which(block_trials$event_type == dup_values[d])
      block_trials[ev_rows,] <- rep_ev
      # TODO issue with repeatTrial in WPT that it gets repeated after each display_both_stimuli. Need to figure out how to represent this trial. This is might be solved for situations where we input the behav data
    }
    # block_trials <- block_trials %>% select(-ntrials) # removing ntrials since it has been incorporated in the table by repeating the events the expected number of times
  }
  return(block_trials)
}



#' @title build a dataframe of all expected events and trials in a block by matching the behav data with the events in the config file
#' @param exptd_trial contains the exptd_events dataframe for a trial.
#' @param behav dataframe of the behavioural datafile with rows for each event for all trials and blocks and columns containing the data ex: RTs, trial conditions, responses, etc.. 
#'     One column needs to be reference column with eventid to match the config and behav file rows. eventid is the column in exptd_trial that matches with the behav file column mentioned in event_col.
#' @param event_col is the column name in the behave file which contains event names for each row of data. This will be matched with the eventid column in exptd_trial.
#' @param nblocks number of repetitions of the current block, so that the events and trials for only one repetition is present in the output.
#' 
#' @return block_trials dataframe of all events and trials in a block created based on the config file
#'
#' @importFrom dplyr %>% mutate row_number filter left_join pull arrange select
#'
#' @author Nidhi Desai
#'   
event_seq_match_behav <- function(exptd_trial, behav, event_col, nblocks){
  # replace the column name for event types in the behav file for easy accessibility in the code below
  colnames(behav)[colnames(behav) == event_col] <- "eventid"
  behav <- behav %>% mutate(row_number = row_number())
  
  # We are creating a table of row_numbers from the behav data and the event that those correspond to
  # for each event find the row numbers corresponding to that event and add to this table and add the event number (row in exptd_trial) to the next column
  # at the end sort the row number column in ascending order and now we can replace the rows from exptd_trial into this table in the sequence of events found in behav file.
  block_trials <- data.frame(behav_nrow = as.numeric(), exptd_nrow = as.numeric())
  for (r in seq_along(exptd_trial$event_type)){ # for each event_type
    # filtering the behav data for the current event based on behav_file_rows
    rows_list <- behav %>% filter(eventid == exptd_trial$behav_eventid[r]) %>% pull(row_number)
    if (length(rows_list) == 0) { next }
    # if (!is.na(exptd_trial$behav_find_rows[r][1])){
    # frequency is still being calculated from the config file, so we don't need to extract frequency from the behav datafile
    for (y in seq_along(exptd_trial$behav_find_rows[[r]])){
      rows_list <- behav %>%
        filter(row_number %in% rows_list) %>%
        filter(!!sym(names(exptd_trial$behav_find_rows[[r]][y])) == exptd_trial$behav_find_rows[[r]][[y]]) %>%
        pull(row_number)
    }
    if (length(rows_list) == 0) { next }
    # }
    block_trials <- rbind(block_trials, data.frame(behav_nrow = rows_list, exptd_nrow = r))
  }
  block_trials <- block_trials %>% arrange(behav_nrow)
  
  # These events are for all nblocks*ntrials*nevents
  # The block_trials can be divided into row numbers for separate repetitions of blocks by separating the rows into nblocks pieces
  block_trials <- block_trials[1:(nrow(block_trials)/nblocks),]
  
  # replace the event numbers with rows of events from exptd_trial
  exptd_trial <- exptd_trial %>% mutate(row_num = row_number())
  block_trials <- block_trials %>% 
    left_join(exptd_trial, by = c("exptd_nrow" = "row_num")) %>% 
    select(-exptd_nrow)
  
  return(block_trials)
}



#' @title get a dataframe containing the blocks and corresponding sequence numbers in the phase based on the block sequence in the behavioral data file
#' @description config file will need nblocks to be set under phase$blocks incase number of repetitions of a 
#'     block is greater than 1 to build a phase incase only the name of the block is mentioned it is assumed that nblocks=1
#'
#' @param nblocks_list list of number of expected blocks in this phase with the corresponding names of these blocks
#' @param phase name of the current phase for which the block sequence is being determined
#' @param behav_subset_var list of the column names from behav files which are used to filter the rows of the behav data and the shortcut letters assigned to them in the config file
#' @param expstruct_f the experiment structure defined in the config file
#' @param behav_f dataframe of the loaded behavioral data file
#' 
#' @return block_seq_df a dataframe with block_name and block_num columns with rows for each block that is expected to be present in this phase.
#' 
#' @importFrom dplyr %>% add_row mutate row_number filter arrange select
#' @importFrom tibble tibble
#'
#' @author Nidhi Desai
#' 
get_block_seq_match_behav <- function(nblocks_list, phase, behav_subset_var,
                                      expstruct_f, behav_f){
  
  ## setup empty dataframe to add block names and the expected sequence number
  block_seq_df <- data.frame(matrix(ncol = 3, nrow = 0))
  names(block_seq_df) <- c("block_name", "block_num", "block_rownum")
  block_seq_df$block_name <- as.character(block_seq_df$block_name)
  
  ## Find the sequence of blocks based on behav data
  for (j in seq_along(nblocks_list)){ # going through each block
    block <- names(nblocks_list[j])
    block_level <- expstruct$blocks[[block]]
    block_seq_vals <- if_null(block_level$block_seq_val) # all trials and events inside this block can be found by using these behav column values 
    seq_col_val <- extract_subset_vals(behav_subset_var, block_seq_vals)
    subset_behav <- behav_f %>% mutate(row_number = row_number())
    for (k in names(seq_col_val)){
      subset_behav <- subset_behav %>% filter(.data[[k]] == seq_col_val[[k]])
    }
    
    ## one subset of behav file will contains rows for all repetitions of the block
    # Those can be distinguish using the column in behav file which counts the blocks in a phase
    # This is not always necessary but incase there are multiple repetitions of a block this becomes necessary to distinguish them out from each other.
    
    if (!is.null(expstruct_f$phases[[phase]]$block_counter)){ # this is set for an entire phase
      # subset_behav contains data for multiple repetitions of a block, if exists i.e. if (nblocks_list[[j]] > 1) 
      block_seq_df <- block_seq_df %>% add_row(tibble(block_name = block, block_num = unique(subset_behav[[expstruct_f$phases[[phase]]$block_counter]])))
    } else {
      # need a way to find where this block is in the sequence of blocks, so that we dont need the block_counter variable
      block_seq_df <- block_seq_df %>% add_row(tibble(block_name = block, block_rownum = subset_behav$row_number[1]))
    }
  }
  
  ## add block_number based on the block's events' row number in behav file
  if (is.null(expstruct$phases[[phase]]$block_counter)){
    block_seq_df <- block_seq_df %>% arrange(block_rownum) %>% mutate(block_num = row_number())
  }
  block_seq_df <- block_seq_df %>% select(-block_rownum) %>% arrange(block_num)
  
  return(block_seq_df)
}



#' @title get a dataframe containing the blocks and corresponding sequence numbers in the phase based on the sequence of blocks mentioned in the config file
#' @param nblocks_list list of number of expected blocks in this phase with the corresponding names of these blocks
#' 
#' @return block_seq_df a dataframe with block_name and block_num columns with rows for each block that is expected to be present in this phase.
#'
#' @importFrom dplyr %>% add_row mutate
#'
#' @author Nidhi Desai
#' 
get_block_seq_match_config <- function(nblocks_list){
  block_seq_df <- data.frame(matrix(ncol = 2, nrow = 0))
  names(block_seq_df) <- c("block_name", "block_num")
  block_seq_df$block_name <- as.character(block_seq_df$block_name)
  for (j in seq_along(nblocks_list)){ # going through each block
    for (y in 1:nblocks_list[[j]]){
      block_seq_df <- block_seq_df %>% add_row(tibble(block_name = names(nblocks_list)[j])) 
    }
  }
  block_seq_df <- block_seq_df %>% mutate(block_num = row_number()) # add block_number based on the block's events' row number in behav file
  return(block_seq_df)
}




# ========================== additional helper functions for small adjustments =========================== #

#' @title match the behavioral data column names to their expected values in a block/trial
#' 
#' @description matches the variables in behavioral data, which are assigned to a letter in the definitions$behav section of the 
#'  config file, to a numeric value there variables are expected to hold in a particular block/trial
#'
#' @param behav_subset_var list of behavior subset variables
#' @param vals character vector of values in the format "variable letter = value"
#'
#' @return a named list of variables and their expected values
#'
#' @examples
#' # Example usage
#' behav_subset_var <- list(a = "pavStimuliConditioning", b = "stimuliSocialOrNot", c = "pitResponse_csCondition")
#' vals <- c("a = 2", "b = 6", "c = 4")
#' # Returns: list(pavStimuliConditioning = 2, stimuliSocialOrNot = 6, pitResponse_csCondition = 4)
#' 
#' @author Nidhi Desai
#'
extract_subset_vals <- function(behav_subset_var, vals){
  subset_ls <- lapply(vals, function(s){ as.numeric(trimws(str_split(s, "=")[[1]][2])) }) 
  match_letters <- unlist(lapply(vals, function(s){ trimws(str_split(s, "=")[[1]][1]) }))
  subset_ls <- setNames(subset_ls, unlist(lapply(match_letters, function(x){ behav_subset_var[[x]] })))
  return(subset_ls)
}


#' @title Set NA if input is NULL, otherwise set to input value
#' @description This function takes an input value and checks if it is NULL. If the input is NULL, it returns NA, otherwise it returns the input value.
#' @param x The input value to be checked
#'
#' @return Returns NA if the input is NULL, otherwise returns the input value.
#'
#' @author Nidhi Desai
#' 
#' @examples
#' set_na_if_null(NULL)
#' # Returns: NA
#' 
#' set_na_if_null(5)
#' # Returns: 5
if_null <- function(x){
  if (is.null(x)){
    return(NA)
  } else{
    return(x)
  }
}


#' @title get the last element of a vector/list
#' @param vec a list of elements
#' 
#' @author Nidhi Desai
#' 
get_last_element <- function(vec){
  if (length(vec) > 1) {
    last_element <- vec[-1]
  } else {
    last_element <- vec
  }
  return(last_element)
}


#' @title checks if a data has repeated rows in a column for the value and outputs the row number if not repeated
#' @param data the table from which the row number needs to be found
#' @param colname name of the column in which the value needs to be matched
#' @param value the value that needs to be found in the specified column in the data table
#' 
#' @author Nidhi Desai 
#' 
get_row_num <- function(data, colname, value){
  temp <- which(data[[colname]] == value)
  if(length(temp) > 1) { 
    print(paste("ERROR: multiple rows in data for", toString(value), "in column", toString(colname)))
    return(0)
  } else if (length(temp) == 0){
    print(paste("ERROR: no rows in data for", toString(value), "in column", toString(colname)))
    return(0)
  } else {
    return(temp)
  }
}


#' @title extracts the list names at any given level inside a hierarchical tree of lists
#' @param tree hierarchical list ex: expstruct read in yaml config file
#' @param depth at which level of depth in the hierarchical tree do the names need to be extracted
#' @param full.names can be set to TRUE or FALSE for getting the entire tree path till the depth or just the final name
#' 
#' @example unique(extract_list_levels(expstruct$exp, 2, TRUE))
#'
#' @author Nidhi Desai 
#' 
extract_list_levels <- function(tree, depth, full.names = FALSE){
  t <- names(unlist(tree, recursive = TRUE, use.names = TRUE))
  names_list <- unlist(lapply(t, function(x){
    temp <- unlist(gregexpr("\\.", x))
    nlayers <- length(temp) + 1
    if(depth == 1){
      substr(x, 1, temp[depth]-1)
    } else if(depth < nlayers){
      substr(x, ifelse(full.names, 1, temp[depth-1]+1), temp[depth]-1) 
    } else if(depth == nlayers){
      substr(x, ifelse(full.names, 1, temp[depth-1]+1), nchar(x))
    } else {NA}
  }))
  return(names_list)
}


#' @title add values to a cell in a dataframe based on the columns that should match the new table
#' @param df dataframe to which adding the values
#' @param new_values new dataframe with columns and their values to match in df and the new values in the columns that need to be added to df
#' @param match_cols list of names of columns whose names should be matched with the 
#' @param add_cols list of names of columns whose values need to be added from new_values to df in the new row
#' @param new_row optional input to add the new row at a specific row number in df
#' 
#' @importFrom dplyr mutate slice row_number inner_join pull bind_rows
#' @importFrom tibble add_row
#' 
#' @author Nidhi Desai
#' 
add_values <- function(df, new_values, match_cols, add_cols, new_row = NULL) {
  df <- df %>% mutate(row_num = row_number())
  common_row <- df %>% inner_join(new_values, by = match_cols) %>% pull(row_num)
  if (length(common_row) > 1){
    print("multiple rows in expected data frame with the same values")
    return(0)
  } else if (length(common_row) == 1){
    for (i in add_cols){ df[common_row, i] <- new_values[[i]] }
  } else {
    if (is.null(new_row)){
      df <- df %>% add_row(new_values) %>% mutate(row_num = row_number())
    } else {
      df <- bind_rows(slice(df, 1:new_row-1), new_values, slice(df, new_row:nrow(df))) %>% mutate(row_num = row_number())
    }
  }
  return(df)
}


