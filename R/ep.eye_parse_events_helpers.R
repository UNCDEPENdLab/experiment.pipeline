############################
##### List of subsidiary functions utilized in `ep.eye_parse_events()`
############################
# - ep.eye_parse_event_info()
# - ep.eye_validate_msg_seq()
# -- ep.eye_check_requested_msg()
# -- ep.eye_check_msg_order()
############################


#' split off function for extracting event info
#'
ep.eye_parse_event_info <- function(ep.eye, 
                                    extraction_method,
                                    extract_event_func_path,
                                    csv_path,
                                    msg_seq,
                                    dt) {
  cat(dt)

  #### EXTRACTION METHOD: regexp
  ######## If each event starts with a predictable event/trial indicator followed by separated information about experimental block, block_trial, eventn, and event the package can easily parse this and populate the data with relevant info to be used later.
  if(extraction_method == "regexp"){
    tryCatch.ep({
      # N.B. right now we minimally need an event ID string to search for.
      # If this is not provided, we will not be able to glean any useful about which messages contain information about what is happening in the task and this will painfully need to be recreated on the backend or information from the behavior data will need to be used.
      stopifnot("msg" %in% names(c.e$event_info))

      # # as of the initial draft of this function, only regex functions are used to extract information. Could allow user to specify a separate csv with important information as another option, for example.
      # if(!"extraction_method" %in% names(c.e$event_info)) {
      #   c.e$event_info$extaction_method <- "regex"
      #   cat("\n- 4.2 MESSAGE: No extraction method listed in config, setting to regex\n")
      # }

      ### 3.4.1 ensure all events have a message with the expected msg string
      dt1 <-  "-- 3.4.1 Ensure all events have a message with the expected msg string:"

      info_msgs <- ep.eye$raw %>% dplyr::filter(grepl(c.e$event_info$msg, et.msg))
      info_match <- info_msgs$eventn == unique(ep.eye$raw$eventn)

      #if not every event has an event info message, log in meta data and move on.
      if(!all(info_msgs$eventn == unique(ep.eye$raw$eventn))){
        ep.eye$metadata[["miss_ev_info_msg"]] <- info_msgs$eventn[which(!info_match)]
        stop("Not all events have corresponding event information message")
      }
    }, describe_text = dt1)

    ### 3.4.2 populate raw data with event info
    dt2 <-  "-- 3.4.2 Populate raw data with event info:"
    tryCatch.ep({

      tomerge <- info_msgs %>% rename(`event_info` = `et.msg`) %>% dplyr::select(eventn, event_info)

      if(!"msg_extract" %in% names(c.e$event_info)){
        # msg extract is a regex that tells ep what portion of the event info message needs to be extracted and used to convey information about what is happening on the screen.
        # if blank, just plug the whole string into an event_info column.
        cat("- 3.4.2 MESSAGE: No instructions on what specific event info to extract, placing the entire message in a single column (eye$raw$event_info)\n")

      } else{
        # pairs down to just  what is requested in msg.extract
        tomerge$event_info <- str_extract(tomerge$event_info, c.e$event_info$msg_extract)
        if("split_by" %in% names(c.e$event_info)){ # if information is spliced via a designated seperator.
          if("msg_parts" %in% names(c.e$event_info)){ # parts of messages are designated with column names
            tomerge <- tomerge %>% separate(event_info, c.e$event_info$msg_parts, sep = c.e$event_info$split_by,)
          } else{ # non-descriptive letters
            tomerge <- tomerge %>% separate(event_info, letters[1:length(str_split(tomerge$event_info, pattern = c.e$event_info$split_by)[[1]])], sep = c.e$event_info$split_by,)
          }
        }
      }


      # convert columns to numeric if possible. ARobertson: https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r
      is_all_numeric <- function(x) {
        !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
      }
      tomerge <- tomerge %>% mutate_if(is_all_numeric,as.numeric)

      # final merge to raw, and gev dfs
      ep.eye$raw <- ep.eye$raw %>% left_join(tomerge, by = "eventn")
      ep.eye$gaze$sacc <- ep.eye$gaze$sacc %>% left_join(tomerge, by = "eventn")
      ep.eye$gaze$fix <- ep.eye$gaze$fix %>% left_join(tomerge, by = "eventn")
      ep.eye$gaze$blink <- ep.eye$gaze$blink %>% left_join(tomerge, by = "eventn")

    }, describe_text = dt2)
  } else if(extraction_method == "csv"){
    ### EXTRACTION METHOD: csv
    ######## Sometimes it is simply easier to generate a .csv file linking specific messages to important trial related information that can be easily merged into the ep.eye data in order to be compliant with ep.eye naming conventions
    ######## N.B. event csvs must minimally contain columns "block", "block_trial", "event", "eventn", "time", and "et.msg". At time of writing (4/20/21), I dont see a reason why additional columns couldnt be included but I havent thought deeply about this.

    ### check that all times from event_csv are contained within raw data

    tryCatch.ep({
      dt1 <- "-- 3.4.1 Confirm timing match between csv_path and data and merge:"
      info_msgs <- read.csv(ev_inf$csv_path)
      info_msgs$time <- info_msgs$time - ep.eye$metadata$t_start
      stopifnot(all(info_msgs$time %in% ep.eye$raw$time))
    }, describe_text = dt1)

    tryCatch.ep({
    if(exists("info_msgs")){
      dt2 <- "-- 3.4.2 Merge trial info to ep.eye data:"
      non_join_colnames <- colnames(info_msgs)[which(!colnames(info_msgs) %in% c("eventn", "et.msg", "time"))]

      eye$raw <- eye$raw %>% left_join(info_msgs, by = c("time", "et.msg", "eventn")) %>% group_by(eventn) %>%
        tidyr::fill(all_of(non_join_colnames), .direction = "updown") %>%
        ungroup() %>% data.table()

      eye$gaze$sacc <- eye$gaze$sacc %>% left_join(dplyr::select(info_msgs, -time, -et.msg), by = c("eventn")) %>% group_by(eventn) %>%
        tidyr::fill(all_of(non_join_colnames), .direction = "updown") %>%
        ungroup() %>% data.table()

      eye$gaze$fix <- eye$gaze$fix %>% left_join(dplyr::select(info_msgs, -time, -et.msg), by = c("eventn")) %>% group_by(eventn) %>%
        tidyr::fill(all_of(non_join_colnames), .direction = "updown") %>%
        ungroup() %>% data.table()

      eye$gaze$blink <- eye$gaze$blink %>% left_join(dplyr::select(info_msgs, -time, -et.msg), by = c("eventn")) %>% group_by(eventn) %>%
        tidyr::fill(all_of(non_join_colnames), .direction = "updown") %>%
        ungroup() %>% data.table()


      }
    }, describe_text = dt2)


  } else if(extraction_method == "function"){

    ### 4.2.1: Generate data frame 
    tryCatch.ep({
      dt1 <- paste0("-- 4.1.1 Extracting eye events from user-supplied function (",basename(extract_event_func_path) ,"):")
      
      ev_f <- source(extract_event_func_path)$value
      # if csv_path is specified, save the outputs of function into this directory as csvs, otherwise, just read. As a sanity check it is good idea to write and review a couple csvs to see how the event extraction and renaming is working internally. If you are confident the extraction works in your function, go ahead and skip the write.  
      if(!is.null(csv_path)){
        if(!dir.exists(dirname(csv_path))) dir.create(dirname(csv_path), recursive = TRUE)
        info_msgs <- ev_f(ep.eye, csv_path = csv_path)
      } else {
        info_msgs <- ev_f(ep.eye)
      }

    },describe_text = dt1)
    
    tryCatch.ep({
      dt2 <- "-- 4.1.2 Merge trial/event info to eye data:"
      non_join_colnames <- colnames(info_msgs)[which(!colnames(info_msgs) %in% c("eventn", "et.msg", "time"))]

      # In some cases, user functions will add messages where they do not appear in the standard messages. E.g. padding a trial ID from a between-trial message that contains important information. Take care of this here
      ep.eye$raw <- ep.eye$raw %>% left_join(select(info_msgs, time, et.msg) %>% rename(`msg.update` = `et.msg`), by = "time") %>% mutate(et.msg = ifelse(is.na(msg.update), et.msg, msg.update)) %>% select(-msg.update) 
      
      # Probably not the most efficient but gets the job done.
      ep.eye$raw <- ep.eye$raw %>% left_join(info_msgs, by = c("time", "eventn", "et.msg")) %>% group_by(eventn) %>%
        tidyr::fill(all_of(non_join_colnames), .direction = "updown") %>%
        ungroup() %>% data.table() #%>% filter(eventn == 4)

      ep.eye$gaze$sacc <- ep.eye$gaze$sacc %>% left_join(dplyr::select(info_msgs, -time, -et.msg), by = c("eventn")) %>% group_by(eventn) %>%
        tidyr::fill(all_of(non_join_colnames), .direction = "updown") %>%
        ungroup() %>% data.table()

      ep.eye$gaze$fix <- ep.eye$gaze$fix %>% left_join(dplyr::select(info_msgs, -time, -et.msg), by = c("eventn")) %>% group_by(eventn) %>%
        tidyr::fill(all_of(non_join_colnames), .direction = "updown") %>%
        ungroup() %>% data.table()

      ep.eye$gaze$blink <- ep.eye$gaze$blink %>% left_join(dplyr::select(info_msgs, -time, -et.msg), by = c("eventn")) %>% group_by(eventn) %>%
        tidyr::fill(all_of(non_join_colnames), .direction = "updown") %>%
        ungroup() %>% data.table()
    }, describe_text = dt2)


  }


  return(ep.eye)
}


#' split off function for checking message sequence.
#'
ep.eye_validate_msg_seq <- function(ep.eye, 
                                    msg_seq, 
                                    dt){

  cat(dt)
  
  tryCatch.ep({
    msg_check <- data.frame()
    for(i in unique(ep.eye$raw$eventn)){
      df <- ep.eye_check_requested_msg(ep.eye, eventnum = i)
      msg_check <- rbind(msg_check, df)
    }

    message_check_error <- msg_check %>% filter(!match) #%>% na.omit()
    missing_eventnum <- ep.eye$raw$eventn[which(!unique(ep.eye$raw$eventn) %in% msg_check$eventn)]

    if(nrow(message_check_error) != 0) ep.eye$metadata[["validate_msg_seq"]][["message_check_error"]] <- message_check_error
    if(length(missing_eventnum) != 0) ep.eye$metadata[["validate_msg_seq"]][["message_check_missing"]] <- missing_eventnum

  }, describe_text = "-- 4.2.1 Check requested messages per event:")

# Check ordering of messages. This should be skipped unless you are exactly sure that message ordering is exactly as planned.
  dt <- "-- 4.2.2 Check message sequencing:"
  if(msg_seq$ordered){
    tryCatch.ep({
        msg_check_ordered <- data.frame()
        for(i in unique(ep.eye$raw$eventn)){
          if(!i %in% missing_eventnum){
            df <- ep.eye_check_msg_order(ep.eye, 
                                        msg_seq = msg_seq,
                                        msg_check = msg_check,
                                        eventnum = i)
          }
          msg_check_ordered <- rbind(msg_check_ordered, df)
        }
        seq_mismatch <- msg_check_ordered %>% filter(!seq_match)
        if(nrow(seq_mismatch) != 0) ep.eye$metadata[["validate_msg_seq"]][["seq_mismatch"]] <- seq_mismatch
    }, describe_text = dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  # print either success or point to sequence/message mismatches.
  tryCatch.ep({
    if(!is.null(ep.eye$metadata[["validate_msg_seq"]])){
          warning("IMPERFECT MESSAGE SEQUENCES FOUND IN: 'ep.eye$metadata$validate_msg_seq'")
        }
  }, describe_text = "Message Validation:")
    

  return(ep.eye)
}


ep.eye_check_requested_msg <- function(ep.eye, 
                                       eventnum){
  eblock <- ep.eye$raw  %>% dplyr::filter(eventn == eventnum)

  # Generate vector of expected/requested messages to check within eventn i.
  check_these <- msg_seq[[unique(eblock$block)]][[unique(eblock$event)]] #for every eventn there should only be one block designation and one event designation.

  # Generate vector of extracted messages as they appear in the edf file.
  extracted_msgs <- c() 
  for(j in check_these){
    extracted_msgs <- rbind(extracted_msgs, eblock %>% dplyr::filter(grepl(j, et.msg)) %>% select(time, et.msg)) %>%
      arrange(time) # arrange at the end ensures that extracted messages are stored in the order they appear in continuous time rather than the order they were requested (i.e. the j iterator)
  }
  # pull just the et.msgs
  extracted_msgs <- unique(extracted_msgs$et.msg)

  # Perform comparison of expected and extracted messages. This generated a df with columns c("requested", "match", and "extracted") which provide a mapping between all messages that should be within a given event and the actual message that is extracted from the .edf file. all(match) == TRUE indicates that all requested messages are contained within the et.msgs in the .edf file (good thing).  
  # Note. Output df will be ordered according to the ordering of expected messages encoded in check_these. Though, extracted will be mutated "to" the user's expectations, thus the ordering of extracted messages could be incorrect.
  if(!is.null(extracted_msgs)){
      cvec <- sapply(check_these, function(x) grepl(x, extracted_msgs))
    df <- data.frame(apply(cvec, 2, function(x) any(x))) %>% rownames_to_column() # right column will throw FALSE if there are no matches in the extracted strings, meaning the message is missing in the .edf file.
    colnames(df) <- c("requested", "match")
    df <- df %>% group_by(requested) %>%  mutate(extracted = ifelse(match, extracted_msgs[grepl(requested, extracted_msgs)], NA)) %>% ungroup() %>% mutate(eventn = eventnum) %>% select(eventn, requested, match, extracted)
    return(df)
  } 
 }
      
ep.eye_check_msg_order <- function(ep.eye,
                                   msg_seq,
                                   msg_check,
                                   eventnum){
    # Create df with new column that expects the sequences of messages to match in order, this will be reset to FALSE if later checks denote a discrepancy.                                     
    df <- msg_check %>% filter(eventn == eventnum) %>% mutate(seq_match = TRUE)
    
    
    # Check start and end message sequence. Messages passed in the middle of the event will be evaluated with respect to start and end expectancies in the second step. 
    # User must specify msg_seq$msg_start and msg_seq$msg_end for this check to run. This assumes that messages are passed to the eyetracker in the exact order they are specified in these two fields. 
    start_end_seq <- c(msg_seq$msg_start, msg_seq$msg_end)
    
    # Get ordering of requested messages as they appear in raw data.
    raw_seq <- ep.eye$raw %>% filter(eventn == eventnum, et.msg != ".") %>% pull(et.msg) %>% unique()
    raw_seq_start_end <- raw_seq[which(grepl(paste(start_end_seq, collapse="|"), raw_seq))]

    for(j in 1:length(raw_seq_start_end)){
      df[df$requested == start_end_seq[j], "seq_match"] <- grepl(start_end_seq[j], raw_seq_start_end[j])
    }

    ## N.B. This does not check "mid_msgs"

  return(df)
}

# # Check ordering of messages. This should be skipped unless you are exactly sure that message ordering is exactly as planned.
#   dt <- "-- 4.3.2 Check message sequencing:"
#   if(msg_seq$ordered){
#     tryCatch.ep({
#         msg_check_ordered <- data.frame()
#         for(i in unique(ep.eye$raw$eventn)){
#           if(!i %in% missing_eventnum){
#             df <- ep.eye_check_msg_order(ep.eye, 
#                                         msg_seq = msg_seq,
#                                         msg_check = msg_check,
#                                         eventnum = i)
#           }
#           msg_check_ordered <- rbind(msg_check_ordered, df)
#         }
#         seq_mismatch <- msg_check_ordered %>% filter(!seq_match)
#         if(nrow(seq_mismatch) != 0) ep.eye$metadata[["validate_msg_seq"]][["seq_mismatch"]] <- seq_mismatch
#     }, describe_text = dt)
#   } else{
#     cat(paste0(dt, " SKIP\n"))
#   }

#   # print either success or point to sequence/message mismatches.
#   tryCatch.ep({
#     if(!is.null(ep.eye$metadata[["validate_msg_seq"]])){
#           warning("IMPERFECT MESSAGE SEQUENCES FOUND IN: 'ep.eye$metadata$validate_msg_seq'")
#         }
#   }, describe_text = "Message Validation:")
    

#   return(ep.eye)
# }


# ep.eye_check_requested_msg <- function(ep.eye, 
#                                        eventnum){
#   eblock <- ep.eye$raw  %>% dplyr::filter(eventn == eventnum)

#   # Generate vector of expected/requested messages to check within eventn i.
#   check_these <- msg_seq[[unique(eblock$block)]][[unique(eblock$event)]] #for every eventn there should only be one block designation and one event designation.

#   # Generate vector of extracted messages as they appear in the edf file.
#   extracted_msgs <- c() 
#   for(j in check_these){
#     extracted_msgs <- rbind(extracted_msgs, eblock %>% dplyr::filter(grepl(j, et.msg)) %>% select(time, et.msg)) %>%
#       arrange(time) # arrange at the end ensures that extracted messages are stored in the order they appear in continuous time rather than the order they were requested (i.e. the j iterator)
#   }
#   # pull just the et.msgs
#   extracted_msgs <- unique(extracted_msgs$et.msg)

#   # Perform comparison of expected and extracted messages. This generated a df with columns c("requested", "match", and "extracted") which provide a mapping between all messages that should be within a given event and the actual message that is extracted from the .edf file. all(match) == TRUE indicates that all requested messages are contained within the et.msgs in the .edf file (good thing).  
#   # Note. Output df will be ordered according to the ordering of expected messages encoded in check_these. Though, extracted will be mutated "to" the user's expectations, thus the ordering of extracted messages could be incorrect.
#   if(!is.null(extracted_msgs)){
#       cvec <- sapply(check_these, function(x) grepl(x, extracted_msgs))
#     df <- data.frame(apply(cvec, 2, function(x) any(x))) %>% rownames_to_column() # right column will throw FALSE if there are no matches in the extracted strings, meaning the message is missing in the .edf file.
#     colnames(df) <- c("requested", "match")
#     df <- df %>% group_by(requested) %>%  mutate(extracted = ifelse(match, extracted_msgs[grepl(requested, extracted_msgs)], NA)) %>% ungroup() %>% mutate(eventn = eventnum) %>% select(eventn, requested, match, extracted)
#     return(df)
#   } 
#  }
      
# ep.eye_check_msg_order <- function(ep.eye,
#                                    msg_seq,
#                                    msg_check,
#                                    eventnum){
#     # Create df with new column that expects the sequences of messages to match in order, this will be reset to FALSE if later checks denote a discrepancy.                                     
#     df <- msg_check %>% filter(eventn == eventnum) %>% mutate(seq_match = TRUE)
    
    
#     # Check start and end message sequence. Messages passed in the middle of the event will be evaluated with respect to start and end expectancies in the second step. 
#     # User must specify msg_seq$msg_start and msg_seq$msg_end for this check to run. This assumes that messages are passed to the eyetracker in the exact order they are specified in these two fields. 
#     start_end_seq <- c(msg_seq$msg_start, msg_seq$msg_end)
    
#     # Get ordering of requested messages as they appear in raw data.
#     raw_seq <- ep.eye$raw %>% filter(eventn == eventnum, et.msg != ".") %>% pull(et.msg) %>% unique()
#     raw_seq_start_end <- raw_seq[which(grepl(paste(start_end_seq, collapse="|"), raw_seq))]

#     for(j in 1:length(raw_seq_start_end)){
#       df[df$requested == start_end_seq[j], "seq_match"] <- grepl(start_end_seq[j], raw_seq_start_end[j])
#     }

#     ## N.B. This does not check "mid_msgs"

#   return(df)
# }
