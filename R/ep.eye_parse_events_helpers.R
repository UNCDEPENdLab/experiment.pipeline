############################
##### List of subsidiary functions utilized in `ep.eye_parse_events()`
############################
# - ep.eye_parse_event_info()
# - ep.eye_validate_msg_seq()
# -- ep.eye_check_requested_msg()
# -- ep.eye_check_msg_order()
############################


#' @title Extract descriptive information about events based on used-defined function
#'
#' @param ep.eye ep.eye object that has been previously initialized and tidied
#' @param extract_event_func_path Path to user-defined message parsing function here
#' @param csv_path Path to write event .csvs to
#' @param msg_seq List of optional message sequence arguments passed in config file. Can contain \code{msg_start}, \code{msg_end}, \code{eval_middle}, \code{ordered}. See the ep.eye_config vignette for details on these fields.
#' @param dt  descriptive text to print.
#'
#' @return ep.eye appended with important event-level information included.
#'
#' @author Nate Hall
#' @export
ep.eye_parse_event_info <- function(ep.eye,
                                    extract_event_func_path,
                                    csv_path = NULL,
                                    msg_seq,
                                    dt = NULL) {
  # browser()
  # debug:
  # -----
  # ep.eye <- eye_init
  # extract_event_func_path = config$definitions$eye$msg_parse$extract_event_func_path
  # csv_path = file.path(config$definitions$eye$msg_parse$csv_dir_path, paste0(config$definitions$eye$global$prefix, ".csv"))
  # msg_seq = config$definitions$eye$msg_parse$msg_seq
  # dt <- "- 3.1 Parsing event information:\n"
  # -----
  #
  cat(dt)

  ### 4.2.1: Generate data frame
    dt1 <- paste0("-- 3.1.1 Extracting eye events from user-supplied function (",basename(extract_event_func_path) ,"):")
  tryCatch.ep({

    ev_f <- source(extract_event_func_path)$value
    # if csv_path is specified, save the outputs of function into this directory as csvs, otherwise, just read. As a sanity check it is good idea to write and review a couple csvs to see how the event extraction and renaming is working internally. If you are confident the extraction works in your function, go ahead and skip the write.
    if(!is.null(csv_path)){
      if(!dir.exists(dirname(csv_path))) dir.create(dirname(csv_path), recursive = TRUE)
      info_msgs <- ev_f(ep.eye, csv_path = csv_path)
    } else {
      info_msgs <- ev_f(ep.eye)
    }

    # convert columns to numeric if applicable
    info_msgs <- suppressWarnings(info_msgs %>%
                                    mutate_all(type.convert) %>%
                                    mutate_if(is.factor, as.character))
    # add info_msgs to metadata
    ep.eye$metadata[["eye_trial_msgs"]] <- info_msgs

  },describe_text = dt1)

    dt2 <- "-- 3.1.2 Merge trial/event info to eye data:"
  tryCatch.ep({

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





  return(ep.eye)
}


#' @title Validate the presence of task-general messages by event
#'
#' @param ep.eye ep.eye object that has been previously initialized and tidied
#' @param msg_seq List of optional message sequence arguments passed in config file. Can contain \code{msg_start}, \code{msg_end}, \code{eval_middle}, \code{ordered}. See the ep.eye_config vignette for details on these fields.
#' @param dt  descriptive text to print
#'
#' @return ep.eye
#'
#' @author Nate Hall
#' @export
ep.eye_validate_msg_seq <- function(ep.eye,
                                    msg_seq,
                                    dt){
# browser()
  cat(dt)

  tryCatch.ep({
    msg_check <- data.frame()
    for(i in unique(ep.eye$raw$eventn)){
      df <- ep.eye_check_requested_msg(ep.eye, msg_seq, eventnum = i)
      msg_check <- rbind(msg_check, df)
    }

    message_check_error <- msg_check %>% filter(!match) #%>% na.omit()
    missing_eventnum <- ep.eye$raw$eventn[which(!unique(ep.eye$raw$eventn) %in% msg_check$eventn)]

    if(nrow(message_check_error) != 0) ep.eye$metadata[["validate_msg_seq"]][["message_check_error"]] <- message_check_error
    if(length(missing_eventnum) != 0) ep.eye$metadata[["validate_msg_seq"]][["message_check_missing"]] <- missing_eventnum

  }, describe_text = "-- 3.2.1 Check requested messages per event:")

  # Check ordering of messages. This should be skipped unless you are exactly sure that message ordering is exactly as planned.
  dt <- "-- 3.2.2 Check message sequencing:"
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


#' @title Compare requested (from config) and extracted (from ep.eye$raw) messages for completeness within a single event
#'
#' @param ep.eye ep.eye object that has been previously initialized and tidied
#' @param eventnum numeric value of the event number to check
#' @param msg_seq List of optional message sequence arguments passed in config file. Can contain \code{msg_start}, \code{msg_end}, \code{eval_middle}, \code{ordered}. See the ep.eye_config vignette for details on these fields.
#' @return df a data.frame with columns c("requested", "match", and "extracted") which provide a mapping between all messages that should be within a given event and the actual message that is extracted from the .edf file. all(match) == TRUE indicates that all requested messages are contained within the et.msgs in the .edf file (good thing).
#'
#' @author Nate Hall
#' @export

ep.eye_check_requested_msg <- function(ep.eye,
                                       msg_seq,
                                       eventnum){
  # browser()
  eblock <- ep.eye$raw  %>% dplyr::filter(eventn == eventnum)

  # Generate vector of expected/requested messages to check within eventn i.
  check_these <- msg_seq[[unique(eblock$block)]][[unique(eblock$event)]] #for every eventn there should only be one block designation and one event designation.

  msg_seq[["trial"]][[unique(eblock$event)]]

  # Generate vector of extracted messages as they appear in the edf file.
  extracted_msgs <- c()
  for(j in check_these){
    extracted_msgs <- rbind(extracted_msgs, eblock %>% dplyr::filter(grepl(j, et.msg)) %>% select(time, et.msg)) %>%
      arrange(time) %>% distinct()# arrange at the end ensures that extracted messages are stored in the order they appear in continuous time rather than the order they were requested (i.e. the j iterator)
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

#' @title Check ordering of messages
#'
#' @param ep.eye ep.eye object that has been previously initialized and tidied
#' @param msg_seq List of optional message sequence arguments passed in config file. Can contain \code{msg_start}, \code{msg_end}, \code{eval_middle}, \code{ordered}. See the ep.eye_config vignette for details on these fields.
#' @param msg_check data.frame generated from \code{ep.eye_check_requested_msg}
#' @param eventnum numeric value of the event number to check
#'
#' @return df msg_check with seq_match appended as column name.
#'
#' @author Nate Hall
#' @export

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
