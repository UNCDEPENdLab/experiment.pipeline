# Parse task config file, extracting essential task information

#' @param eye ep.eye object that has been previously initialized and tidied
#' @param config list that is generated from reading task.yaml
#'


parse_config_eye <- function(eye, config, header = "3. Parse config file for ep.eye:"){#, .c = 3){
  tictoc::tic(); #eye <- eye_init
  if (!"ep.eye" %in% class(eye)) { stop("parse_config_eye expects a pre-initialized ep.eye object") }

  log_chunk_header(header)

  ### 3.1 Extract eye definitions
  dt <- "- 3.1 Extract eye definitions for processing:"
  c.e <- tidy_eye_config(config, dt)

  ### 3.2 Check metadata
  dt <- "- 3.2 Check metadata:"
  if("meta_check" %in% names(c.e)){
    meta_check(c.e, eye, dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 3.3 Extract important between-trial messages if requested
  dt <- "- 3.3 Extract important between-trial messages:\n"
  if("inherit_btw_tr" %in% names(c.e)){
    eye <-  handle_between_trial(c.e, eye, dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 3.4 Extract event info
  dt <- "- 3.4 Parsing event information:\n"
  if("event_info" %in% names(c.e)){
    eye <- get_event_info(c.e, eye, dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }

  ### 3.5 Event sequences check
  dt <- "- 3.5 Check event message sequence and ordering:"
  if("msg_seq" %in% names(c.e$event_info)){
    eye <- check_msg_seq(c.e, eye, dt)
  } else{
    cat(paste0(dt, " SKIP\n"))
  }



  # Include this as an outside step from config parsing, to allow for different options for how AOI inputs to become known to the eye object.

  # ### 3.6 AOI parsing
  # dt <- "- 3.4 Config-based AOI parsing:"
  # if("msg_seq" %in% names(c.e$event_info)){
  #   eye <- aoi_parse(c.e, eye, dt)
  # } else{
  #   cat(paste0(dt, " SKIP\n"))
  # }


  return(eye)

}


#' split off function for checking message sequence.
#'
check_msg_seq <- function(c.e, eye, dt){

  tryCatch.ep({

    ## N.B. by looping over block any errors at any point will stop the script. May be better if this turns into an issue to instead store everything in one large df across blocks as we loop over and check this at the very end. Can return to this later.
    seq_errs <- list()
    for(i in unique(eye$raw$eventn)){
      eblock <- eye$raw %>% dplyr::filter(eventn == i)

      check_these <- c.e$event_info$msg_seq[[unique(eblock$block)]][[unique(eblock$event)]] #for every eventn there should only be one block designation and one event designation.

      # ugly, but works.
      extracted_msgs <- c() #as they appear in the edf file.
      # browser()
      for(j in check_these){
        extracted_msgs <- rbind(extracted_msgs, eblock %>% dplyr::filter(grepl(j, et.msg)) %>% select(time, et.msg)) %>%
          arrange(time) # arrange at the end ensures that extracted messages are stored in the order they appear in continuous time rather than the order they were requested (i.e. the j iterator)
      }

      extracted_msgs <- extracted_msgs$et.msg


      # if(length(extracted_msgs) != length(check_these)){
      # N.B. initially, this was just to set up the df in instances in which the expected msgs and extracted msgs did not match, and to flag missing messages with an NA for later inspection.
      # now, I think this is a more general way to setup such a comparison df.

      # output df will be ordered according to the ordering of expected messages encoded in check_these. Though, extracted will be mutated "to" the user's expectations, thus the ordering of extracted messages could be incorrect.
      cvec <- sapply(check_these, function(x) grepl(x, extracted_msgs))
      df <- data.frame(apply(cvec, 2, function(x) any(x))) %>% rownames_to_column() # right column will throw FALSE if there are no matches in the extracted strings, meaning the message is missing.
      colnames(df) <- c("requested", "match")
      df <- df %>% group_by(requested) %>%  mutate(extracted = ifelse(match, extracted_msgs[grepl(requested, extracted_msgs)], NA)) %>% ungroup()

      # }

      # now, if requested, check the ordering and get mad at user if extracted msgs are out of the expected order.
      # do this by utilizing the initial vector of extracted messages.
      if(c.e$event_info$msg_seq$ordered){
        # this is almost certainly error-prone, can come back to this later. on first glance it seems like it could work in the mean time.
        df <- df %>% left_join(data.frame(extracted = extracted_msgs,
                                          order_check = extracted_msgs == na.omit(df$extracted)), by = "extracted")

      } else {
        df <- df %>% mutate(order_check = TRUE) # don't cause any trouble.
      }

      df <- df %>% mutate(eventn = unique(eblock$eventn),
                          block = unique(eblock$block),
                          block_trial = unique(eblock$block_trial),
                          event = unique(eblock$event)) %>% select(eventn, requested, extracted, match,order_check, block, block_trial, event)

      # print(df)
      if(!all(df$match) | !all(df$order_check)){
        seq_errs[[i]] <- df
      }
    }

    # store imperfect seq matches and print warning. Update to include empty list if all successful and just print warning if there is an issue.
    eye$metadata[["msg_seq_errs"]] <- seq_errs

    if(length(seq_errs) != 0){
      warning("IMPERFECT MESSAGE SEQUENCES FOUND IN: 'ep.eye$metadata$msg_seq_errs'")
    }


  }, describe_text = dt)

  return(eye)
}


#' split off function for extracting event info
#'
get_event_info <- function(c.e, eye, dt){
  cat(dt)
  tryCatch.ep({
    # N.B. right now we minimally need an event ID string to search for.
    # If this is not provided, we will not be able to glean any useful about which messages contain information about what is happening in the task and this will painfully need to be recreated on the backend or information from the behavior data will need to be used.
    stopifnot("msg" %in% names(c.e$event_info))

    # as of the initial draft of this function, only regex functions are used to extract information. Could allow user to specify a separate csv with important information as another option, for example.
    if(!"extraction_method" %in% names(c.e$event_info)) {
      c.e$event_info$extaction_method <- "regex"
      cat("\n- 3.4 MESSAGE: No extraction method listed in config, setting to regex\n")
    }

    ### 3.4.1 ensure all events have a message with the expected msg string
    dt1 <-  "-- 3.4.1 Ensure all events have a message with the expected msg string:"

    info_msgs <- eye$raw %>% dplyr::filter(grepl(c.e$event_info$msg, et.msg))
    info_match <- info_msgs$eventn == unique(eye$raw$eventn)

    #if not every event has an event info message, log in meta data and move on.
    if(!all(info_msgs$eventn == unique(eye$raw$eventn))){
      eye$metadata[["miss_ev_info_msg"]] <- info_msgs$eventn[which(!info_match)]
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
      cat("- 3.4.2 MESSAGE: No instructions on what specific event info extract, placing the entire message in a single column (eye$raw$event_info)\n")

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
    eye$raw <- eye$raw %>% left_join(tomerge, by = "eventn")
    eye$gaze$sacc <- eye$gaze$sacc %>% left_join(tomerge, by = "eventn")
    eye$gaze$fix <- eye$gaze$fix %>% left_join(tomerge, by = "eventn")
    eye$gaze$blink <- eye$gaze$blink %>% left_join(tomerge, by = "eventn")

  }, describe_text = dt2)


  return(eye)
}
#' split off function for conversion of between trial messages to within
#'
handle_between_trial <- function(c.e, eye, dt){
  cat(dt)

  ### 3.3.1 Calibration/validation check
  cat("-- 3.3.1 Calibration/validation checks:\n")
  if("calibration_check" %in% names(c.e$inherit_btw_tr)){
    dt1 <- "--- 3.3.1.1 Calibration:"
    tryCatch.ep({
      c.check <- c.e$inherit_btw_tr$calibration_check$cal
      cal.msg <- eye$metadata$btw_tr_msg %>% dplyr::filter(grepl(c.check, text, fixed = TRUE))
      if(!grepl("GOOD", cal.msg$text)){warning("cal check message does not contain GOOD", call. = FALSE)}
    },
    describe_text = dt1)

    dt2 <- "--- 3.3.1.2 Validation:"
    tryCatch.ep({
      v.check <- c.e$inherit_btw_tr$calibration_check$val
      val.msg <- eye$metadata$btw_tr_msg %>% dplyr::filter(grepl(v.check, text, fixed = TRUE))
      if(!grepl("GOOD", val.msg$text)){warning("val check message does not contain GOOD", call. = FALSE)}
    },
    describe_text = dt2)
  }

  ### 3.3.2 Move requested messages to following event block
  dt3 <- "-- 3.3.2 Pull requested messages into measured data:"
  if("move_to_within" %in% names(c.e$inherit_btw_tr)){
    tryCatch.ep({
      mtw <- c.e$inherit_btw_tr$move_to_within
      stopifnot(all.equal(length(mtw$str), length(mtw$align_msg), length(mtw$pre_post)))
      for(m in 1:length(mtw$str)){
        ms <- mtw$str[m]

        raw_align <- eye$raw %>% dplyr::filter(grepl(mtw$align_msg[m], eye$raw$et.msg))
        instances <- eye$metadata$btw_tr_msg %>% dplyr::filter(grepl(ms, text)) %>%
          group_by(eventn) %>% mutate(eventn = ifelse(!eventn%%1==0, # update 11/12/20: only update eventn if value is not divisible by 0
                                                      ifelse(mtw$pre_post[m] == "pre", # update event depending on pre-post designation
                                                             (eventn + .5),
                                                             eventn - .5),
                                                      eventn)) %>% data.table()

        # every instance of the message in question must have an alignment target.
        stopifnot(nrow(raw_align) == nrow(instances))
        # recode time to immediately after alignment message (search "down" until reaching a point with no messages (coded as "."))
        for(i in 1:nrow(raw_align)){
          t1 <- as.numeric(raw_align[i, "time"])
          msg <- eye$raw %>% dplyr::filter(time == t1) %>% select(et.msg) %>% as.character()
          while (msg != "."){ # search down until hitting first "."
            t1 <- t1 + 1
            msg <- eye$raw %>% dplyr::filter(time == t1) %>% select(et.msg) %>% as.character()
          }
          instances[i,"time"] <- t1
        }
        eye$raw <- eye$raw %>% left_join(instances, by = c("eventn", "time")) %>%  mutate(et.msg = ifelse(!is.na(text), text, et.msg)) %>% select(-text)
      }


    },
    describe_text = dt3)

  }

  return(eye)
}
#' split off function for checking metadata
#'
meta_check <-  function(c.e, eye, dt){
  cat(dt,"\n")

  ### 3.2.1 meta_vars and vals
  dt1 <- "-- 3.2.1 Compare .edf info (session parameters) to expectations:"
  tryCatch.ep({
    stopifnot(all(c("meta_vars", "meta_vals") %in% names(c.e$meta_check)) & length(c.e$meta_check$meta_vars) == length(c.e$meta_check$meta_vals))

    meta_ref <- data.frame("meta_vars" = c.e$meta_check$meta_vars,
                           "meta_vals" = c.e$meta_check$meta_vals) %>% mutate_all(as.character)

    mismatch <- c() # append if any discrepancies.
    for(i in 1:nrow(meta_ref)){
      # message(eye$metadata[[meta_ref[i,"meta_vars"]]], " ", meta_ref[i,"meta_vals"])
      if(!eye$metadata[[meta_ref[i,"meta_vars"]]] == meta_ref[i,"meta_vals"]){mismatch <- c(mismatch, i)}
    }

    if(!is.null(mismatch)){
      warning(mismatch,call. = FALSE)
    }
  },
  describe_text = dt1)

  ### 3.2.2 confirm acceptable session length
  dt2 <- "-- 3.2.2 Compare recording time (session length) to expectations:"
  tryCatch.ep({
    stopifnot("recording_time" %in% names(c.e$meta_check))

    rt_range <- c(c.e$meta_check$recording_time[1] - c.e$meta_check$recording_time[2],
                  c.e$meta_check$recording_time[1] + c.e$meta_check$recording_time[2])

    if(!all((rt_range[1] <= eye$metadata$recording_time) & (eye$metadata$recording_time <= rt_range[2]))){
      warning("session length (", eye$metadata$recording_time,") outside of expected bounds: ", paste0(rt_range, collapse = ", "), call. = FALSE)
    }
  },
  describe_text = dt2)

}
#' split off function for cleaning up standard config setup.
#'
tidy_eye_config <- function(config, dt = NULL){
  tryCatch.ep({
    c.e <- config[["definitions"]][["eye"]]
    if("msg_seq" %in% names(c.e$event_info)){
      if("eval_middle" %in% names(c.e$event_info$msg_seq)){
        for(i in names(config$blocks)){
          # check first for an eye field in each event type in a block.
          for(j in names(config$blocks[[i]]$events)){
            ev_m <- config$blocks[[i]]$events[[j]]$eye
            if(c.e$event_info$msg_seq$ordered){
              msg_vec <- c(c.e$event_info$msg_seq$msg_start, ev_m$mid_msg, c.e$event_info$msg_seq$msg_end)
              c.e$event_info$msg_seq[[i]][[j]] <- msg_vec
            }
          }
        }
      }
    }
  }, describe_text = dt)
  return(c.e)
}
# Run these for testing: --------------------------------------------------


# config <- validate_exp_yaml(yaml_file = yaml_file) #testing neighborhood
eye <- eye_init

config <- validate_exp_yaml(yaml_file = "~/github_repos/experiment.pipeline/inst/examples/yaml_config/shrooms.yaml")
dt <- "- 3.1 Extract eye definitions for processing:"
c.e <- tidy_eye_config(config, dt)

# eye_msg_report(eye)
#######

# eye_msg_report(ep.eye = eye)
# eye$raw <- eye$raw %>% rename(`eventn` = `event`)
# eye$gaze$sacc <- eye$gaze$sacc %>% rename(`eventn` = `event`)
# eye$gaze$fix <- eye$gaze$fix %>% rename(`eventn` = `event`)
# eye$gaze$blink <- eye$gaze$blink %>% rename(`eventn` = `event`)
# eye$metadata$btw_tr_msg <- eye$metadata$btw_tr_msg %>% rename(`eventn` = `event`)
#
