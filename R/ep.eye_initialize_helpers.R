############################
##### List of subsidiary functions utilized in `ep.eye_parse_events()`
############################
# - ep.eye_setup_structure()
# - ep.eye_get_session_length()
# - ep.eye_raw_sample_continuity_check()
# - ep.eye_unify_gaze_events()
# - ep.eye_store_between_event_messages()
# - ep.eye_rm_crinfo()
# - ep.eye_unify_raw_msg()
# - ep.eye_meta_check()
# - ep.eye_shift_timing()
# - ep.eye_inherit_btw_ev()
############################

#' Setup ep.eye structure
#'
#' @param eye Loaded .edf file from \code{read_edf}
#' @param task Task name (character)
#' @param id subject ID (numeric)
#'
#' @return ep.eye Initialized ep.eye structure. [DETAILS HERE].
#' @author Nate Hall
#'
#' @importFrom data.table as.data.table data.table
#'
#' @export
ep.eye_setup_structure <- function(eye, task = NULL, id = NULL){
  ep.eye <- list(raw = eye$raw,
                 msg = eye$msg,
                 gaze = list(downsample = data.table(),
                             sacc = as.data.table(eye$sacc) %>% mutate(saccn = 1:nrow(.)),
                             fix = as.data.table(eye$fix) %>% mutate(fixn = 1:nrow(.)),
                             blink = as.data.table(eye$blinks) %>% mutate(blinkn = 1:nrow(.))),
                 pupil = list(
                   downsample = data.table(),
                   # fix = data.table(),
                   # trial = data.table(),
                   # event = data.table(),
                   preprocessed = data.table()),
                 # summary = list(counts = data.table(),
                 #                sacc = data.table(),
                 #                fix = data.table(),
                 #                blink = data.table(),
                 #                pupil = data.table()
                 #),
                 metadata = suppressWarnings(split(t(eye$info),f = colnames(eye$info)) %>% lapply(., function(x) {
                   if (x %in% c("TRUE", "FALSE")){
                     as.logical(x)} else if(!is.na(as.numeric(x))){
                       as.numeric(x)} else {x}
                 } ))
  )

  # Tag msg with "btw_ev" column to denote whether a message is passed during a recording event or between events.
  # procedure borrowed from: https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
  countDecimalPlaces <- function(x) {
    if ((x %% 1) != 0) {
      strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
      n <- nchar(strs[[1]][2])
    } else {
      n <- 0
    }
    return(n)
  }
  ep.eye$msg$btw_ev <- sapply(ep.eye$msg$eventn, FUN = countDecimalPlaces)

  # append edf filepath, task name and subjectID to metadata
  ep.eye[["metadata"]][["edf_file"]] <- eye$edf_file
  if(!is.null(task)) ep.eye[["metadata"]][["task"]] <- task
  ep.eye[["metadata"]][["id"]] <- id

  class(ep.eye) <- c(class(eye), "ep.eye") #tag with ep.eye class
  return(ep.eye)
}

#' Tag ep.eye metadata with the length of the recording session
#'
#' @param ep.eye Initialized ep.eye structure.
#' @return ep.eye Modified ep.eye structure with metadata$recording time denoting session recording length in seconds.
#'
#' @author Nate Hall
#'
#' @export
ep.eye_get_session_length <- function(ep.eye){

  mintime <- min(ep.eye$raw$time)
  maxtime <- max(ep.eye$raw$time)
  all_time <- seq(mintime,maxtime,1)

  # store overall time for later
  tt <- maxtime-mintime
  ep.eye$metadata$recording_time <- tt_sec <- tt/ep.eye$metadata$sample.rate
  return(ep.eye)
}


#' Tag ep.eye metadata with the length of the recording session
#'
#' @param ep.eye Initialized ep.eye structure.
#' @return ep.eye Modified ep.eye structure with metadata$missing_measurements, either NULL if all timepoints are strictly accounted for or a data.table documenting "runs" of missing data.
#'
#' @author Nate Hall
#'
#' @export
#'
ep.eye_raw_sample_continuity_check <- function(ep.eye){

  mintime <- min(ep.eye$raw$time)
  maxtime <- max(ep.eye$raw$time)
  all_time <- seq(mintime,maxtime,1)

  if(all(all_time %in%  ep.eye$raw$time)){
    ep.eye$metadata[["missing_measurements"]] <- NULL #if everything stricly accounted for (i.e. every sampling period from beginning to end has a measurement... in my experience this is unlikely, esp if between trials the eyetracker is told to stop/start sampling)
  } else{
    #store the gaps for later (maybe).
    mm <- which(!all_time %in% ep.eye$raw$time)  # missing measurements.

    mms <- split(mm, cumsum(c(1, diff(mm) != 1))) # rather than exporting as metadata (too cumbersome) store for input into summary DT.
    mmls <- lapply(mms, function(x) {length(x)}) %>% do.call(c,.) %>% as.numeric() # same as above, just store in summary.
    ep.eye$metadata[["missing_measurements"]] <-  data.table("start" = lapply(mms, function(x) {min(x)}) %>% do.call(c,.),
                                                             "end" = lapply(mms, function(x) {max(x)}) %>% do.call(c,.),
                                                             "length" = mmls)
  }

  ### Check for continuity in events (e.g. that events in time dont jump skip or go out of order)

  # confirmed that unique sorts in order they appear in the array. E.g. y <- c(1,1,3,3,2,3); unique(y) : [1] 1 3 2.
  # will therefor check for skipped events and the ordering.
  stopifnot(all(unique(ep.eye$raw$eventn) == seq(min(unique(ep.eye$raw$eventn)), max(unique(ep.eye$raw$eventn)),1)))

  return(ep.eye)
}

#' Unify gaze events with raw data
#'
#' @description When loaded into the environment, an .edf file will be parsed with fields corresponding to $raw, $sacc, $fix, and $blink. Saccades, fixations, and blinks are called "gaze events" in the ep.eye nomenclature and denote the presence of an event of significance within gaze data. This function unifies raw data with gaze events by generating unique gaze event numbers, merges them to raw data and validates that timestamps from raw and gaze event fields are in correspondence.
#'
#' @param ep.eye An initialized ep.eye object
#' @param gaze_events Character vector of gaze_events to unify with raw. Defaults to unifying sacc, fix, and blink but can be set to any subset of these.
#'
#'
#' @return ep.eye ep.eye structure that has been tagged with gaze event numbers and validated for correspondence between raw and gaze event fields.
#' @author Nate Hall
#'
#' @export
ep.eye_unify_gaze_events <- function(ep.eye,
                                     gaze_events = c("sacc", "fix", "blink")
){
  # debug:
  # -----
  # gaze_events <- c("sacc", "fix", "blink")
  # -----

  # Generate new columns to add gaze event information if requested.
  ep.eye$raw <- cbind(ep.eye$raw, setNames(data.frame(matrix(0, ncol = length(gaze_events), nrow = length(ep.eye$raw$time))), paste0(gaze_events, "n"))) #%>% tibble()

  issues <- list()
  substep <- 0

  ##--------------------------------------------------
  ##  Outer Loop around gaze events (sacc, fix, etc)
  ##--------------------------------------------------

  for(i in gaze_events){

    gaze_event <- ep.eye$gaze[[i]]

    for(row in 1:nrow(gaze_event)){
      # debug:
      # -----
      # row <- 1
      # -----

      # cat(paste0(i, "-", row,"\n"))

      ge <- gaze_event[row,]
      ge_start <- ge %>% pull(stime)
      ge_end <- ge %>% pull(etime)

      gaze_number <- ge %>% pull(!!paste0(i, "n"))

      ep.eye$raw <- ep.eye$raw %>% mutate(!!paste0(i, "n") := case_when((time >= ge_start & time <= ge_end) ~ gaze_number, TRUE ~ get(paste0(i, "n"))))

    }
  }

  return(ep.eye)
}


#' @title Extract messages that are passed between recording events.
#'
#' @description Some task/quality-relevant messages may (depending on how the experiment is setup) be passed to the .edf file in between times where the tracker is actively recording data (e.g. prior to a screen flip or during calibration and validation). This function extracts such messages in ep.eye[["metadata"]]. These "between event messages", are denoted with a eventn ending in .5. For example, messages passed in "eventn" 1.5 are passed between recording events 1 and 2 and may contain information about the previous or following event.
#'
#' @param ep.eye An ep.eye object.
#'
#' @return ep.eye Returns the same ep.eye object, with between-event messages stored in metadata.
#' @export
ep.eye_store_between_event_messages <- function(ep.eye){
  #Store messages with no timestamp match in raw data (collected between trials with no corresponding measurements).
  btw_ev <- ep.eye$msg %>% anti_join(ep.eye$raw, by = "time") %>% data.table()
  ep.eye[["metadata"]][["btw_ev_msg"]] <- btw_ev
  return(ep.eye)
}


#' @title Remove useless cr.info column.
#' @description  Removes cr.info column if it contains unhelpful information
#' @param ep.eye An ep.eye object.
#' @return ep.eye
#' @export
#' @details From Eyelink 100 manual: 4.9.3.1 Samples Recorded in Corneal Reflection Mode: If the data file being processed was recorded using corneal reflection mode, each sample line has an added 3 (monocular) or 5 (binocular) character fields after all other fields (including resolution and velocity if enabled). These fields represent warning messages for that sample relating to the corneal reflection processing.
#'          MONOCULAR Corneal Reflection (CR) Samples
#'            - "..." if no warning for sample
#'            - first character is "I" if sample was interpolated second character is "C" if CR missing
#'            - third character is "R" if CR recovery in progress
ep.eye_rm_crinfo <- function(ep.eye){
  cr <- unique(ep.eye$raw$cr.info)
  if(length(cr) == 1 & "..." %in% cr){
    ep.eye$raw <- ep.eye$raw %>% select(-cr.info)
  } else{
    ep.eye$raw <- ep.eye$raw %>% select(-cr.info)
    warning(paste0("cr.info contains potentially important information (", paste0(cr, collapse = ","), ")"))
  }
  return(ep.eye)
}



#' @title Add et.msg column to raw data
#' @description Adds within-event messages from \code{ep.eye$msg} to \code{ep.eye$raw}.
#' @param ep.eye An ep.eye object.
#'
#' @importFrom data.table setDT
#'
#' @return ep.eye
#'
#' @export
ep.eye_unify_raw_msg <- function(ep.eye){
  # browser()
  ### Update 4/19/22: when data is sampled at <1000Hz, merging messages to the raw data may fail since eyelink messages send at 1000Hz precision despite the eye data being sampled slower.
  ### Added functionality to match message times with the closest timestamp that has an entry in the raw data, which gets stored in an updated $time column in ep.eye$msg. Original timestamps are stored in ep.eye$msg$time_native.
  # if(ep.eye$metadata$sample.rate != 1000){
  raw_times <- ep.eye$raw$time
  mismatch_msg_times <- ep.eye$msg %>% filter(btw_ev == 0) %>% pull(time)
  mismatch_msg_indx <- !mismatch_msg_times %in% raw_times
  if (sum(mismatch_msg_indx) > 0){
    #this identified mismatch between timestamps for messages and raw data (if not collected at 1000Hz)
    # raw_time <- ep.eye$raw %>% filter(eventn == 33) %>% pull(time)
    # msg_time <- ep.eye$msg %>% filter(eventn == 33) %>% pull(time)

    #store old message times in separate name (time_native)
    ep.eye$msg$time_native <- ep.eye$msg$time
    msg_times <- ep.eye$msg$time

    ## add matched timestamps that exist in raw data, must include all.inside = TRUE to ensure vector lengths are equal.
    ep.eye$msg$time_update <- raw_times[findInterval(msg_times, raw_times, all.inside = TRUE)]

    ep.eye$msg <- ep.eye$msg %>% mutate(time = ifelse(btw_ev == 1, time_native, time_update))

    ## spot check. looks good to me.
    # x %>% filter(eventn == 33) %>% print(n = 200)
  }

  ep.eye$raw <- ep.eye$raw %>%
    left_join(dplyr::filter(ep.eye$msg, btw_ev == 0), by = c("eventn", "time")) %>% dplyr::select(-btw_ev) %>%
    dplyr::rename(`et.msg` = `text`) %>%
    mutate(et.msg = ifelse(is.na(et.msg), ".", et.msg)) %>%  select(-any_of(c("time_native", "time_update"))) %>% data.frame()

  # important to back-translate extracted messages to original messages due to the use of left_join. This should not fail.
  umsg <- unique(ep.eye$raw$et.msg)[which(unique(ep.eye$raw$et.msg) != ".")] #unique messages in the final output.

  # make sure to eliminate between-trial messages and just grab messages that are passed while recording.
  umsg_orig <- ep.eye$msg %>% dplyr::filter(btw_ev == 0) %>% pull(text) %>% unique() # unadulterated, right off the eyetracker.


  if(!all(umsg %in% umsg_orig)){
    stop("Backtranslate message merge issue. Errors in this step have not been fully vetted.")
  }

  #   WHYYYYYY
  #   ep.eye$raw <- data.table(data.frame(ep.eye$raw))
  # the below solves the problem (still not sure why we were getting trouble before)
  try({suppressWarnings(setDT(ep.eye$raw))}, silent = TRUE) # need to just suppress the stupid vroom warning, not sure why this is happening.



  return(ep.eye)
  ## vestigial from an earlier version. Not sure if we'll need to revive.

  # if(all(umsg %in% umsg_orig)){
  #   cat("-- 2.8.3 Merge raw gaze data with eyetracker messages, with successful back-translate: SUCCESS\n", sep = "")
  # } else{ # if any mismatch between what is contained in raw data and original message structure, print error and do some digging.

  #   miss_msgs <- umsg[!umsg %in% umsg_orig]
  #   eout$metadata[["missing_messages_raw"]] <- miss_msgs
  #   mmsgs_stamped <- eye$msg[which(eye$msg$text %in% miss_msgs), ]
  #   for(i in 1:nrow(mmsgs_stamped)){
  #     mstamp <- mmsgs_stamped[i,2] #grab missing timestamp.
  #     mstamp %in% mm
  #     mm
  #   }
  #   cat("-- 2.8.3 Merge raw gaze data with eyetracker messages, with successful back-translate: WARNING: errors in this step have not been fully vetted. \n", sep = "")
  # }
}



#' @title Check ep.eye metadata
#'
#' @description When loaded into the environment, an .edf file will be parsed with fields corresponding to $raw, $sacc, $fix, and $blink. Saccades, fixations, and blinks are called "gaze events" in the ep.eye nomenclature and denote the presence of an event of significance within gaze data. This function unifies raw data with gaze events by generating unique gaze event numbers, merges them to raw data and validates that timestamps from raw and gaze event fields are in correspondence.
#'
#' @param ep.eye An initialized ep.eye object.
#' @param meta_vars Character vector of meta variables to check
#' @param meta_vals Character vector of values to expect for the meta variables passed above
#' @param recording_time Numeric vector of length 2 indicating the expected time of the recording session *in seconds* and the margin of error above and below the expected recording time without generating an error.
#' @param dt descriptive text to print to log file, defaults to NULL.
#'
#' @return ep.eye
#' @author Nate Hall
#'
#' @export
ep.eye_meta_check <-  function(ep.eye, meta_vars, meta_vals, recording_time, dt = NULL){
  cat(dt,"\n")

  ### 2.10.1 meta_vars and vals
  dt1 <- "-- 2.10.1 Compare .edf info (session parameters) to expectations:"
  tryCatch.ep({

    stopifnot(!(is.null(meta_vars) | is.null(meta_vals))) # stop if either are null, need both.
    stopifnot(length(meta_vars) == length(meta_vals))


    meta_ref <- data.frame(meta_vars, meta_vals) %>% mutate_all(as.character)

    mismatch <- c() # append if any discrepancies.
    for(i in 1:nrow(meta_ref)){
      # message(eye$metadata[[meta_ref[i,"meta_vars"]]], " ", meta_ref[i,"meta_vals"])
      if(!ep.eye$metadata[[meta_ref[i,"meta_vars"]]] == meta_ref[i,"meta_vals"]){mismatch <- c(mismatch, meta_ref[i,"meta_vars"])}
    }

    if(!is.null(mismatch)){
      ep.eye[["metadata"]][["meta_check"]][["meta_vars_mismatch"]] <- mismatch
      warning(mismatch,call. = FALSE)
    } else {
      ep.eye[["metadata"]][["meta_check"]][["meta_vars_mismatch"]] <- "NONE"
    }

  },
  describe_text = dt1)

  ### 2.10.2 confirm acceptable session length
  dt2 <- "-- 2.10.2 Compare recording time (session length) to expectations:"
  tryCatch.ep({
    stopifnot(!is.null(recording_time))

    rt_range <- c(recording_time[1] - recording_time[2],
                  recording_time[1] + recording_time[2])

    if(!all((rt_range[1] <= ep.eye$metadata$recording_time) & (ep.eye$metadata$recording_time <= rt_range[2]))){
      warning("session length (", ep.eye$metadata$recording_time,") outside of expected bounds: ", paste0(rt_range, collapse = ", "), call. = FALSE)
      ep.eye[["metadata"]][["meta_check"]][["recording_length_violation"]] <- TRUE
    } else{
      ep.eye[["metadata"]][["meta_check"]][["recording_length_violation"]] <- FALSE
    }
  },
  describe_text = dt2)

  return(ep.eye)
}

#' @title Shift timing of ep.eye object to 0 start point
#'
#' @param ep.eye An ep.eye object.
#' @param dt descriptive text to print to log file, defaults to NULL.
#'
#' @return ep.eye ep.eye structure with all relevant elements recoded to 0 start point. Original start point is stored in ep.eye$metadata$t_start
#' @author Nate Hall
#'
#' @export
ep.eye_shift_timing <- function(ep.eye, dt = NULL){
  tryCatch.ep({
    t_start <- ep.eye$raw$time[1]
    ep.eye$metadata$t_start <- t_start

    # raw
    ep.eye$raw$time <- ep.eye$raw$time - t_start
    # saccades
    ep.eye$gaze$sacc$stime <- ep.eye$gaze$sacc$stime - t_start
    ep.eye$gaze$sacc$etime <- ep.eye$gaze$sacc$etime - t_start
    # fixations
    ep.eye$gaze$fix$stime <- ep.eye$gaze$fix$stime - t_start
    ep.eye$gaze$fix$etime <- ep.eye$gaze$fix$etime - t_start
    # blinks
    ep.eye$gaze$blink$stime <- ep.eye$gaze$blink$stime - t_start
    ep.eye$gaze$blink$etime <- ep.eye$gaze$blink$etime - t_start
    # messages
    ep.eye$msg$time <-  ep.eye$msg$time - t_start

    #meta-data
    # ep.eye$metadata$missing_measurements$start <- ep.eye$metadata$missing_measurements$start - t_start
    # ep.eye$metadata$missing_measurements$end <- ep.eye$metadata$missing_measurements$end - t_start
    ep.eye$metadata$btw_ev_msg$time <- ep.eye$metadata$btw_ev_msg$time - t_start



  }, describe_text = dt)
  return(ep.eye)
}


#' @title Interface with messages passed with not recording
#'
#' @description "Between-event messages" are messages passed to the eyetracker while the system is not actively recording. Sometimes these contain event-relevant information that you would like to pull into a recording event itself (for example, a "trial ID" message that gets passed right before the recording starts). This function allows for 1) checking of calibration and validation messages that are passed before any actual recording events are apparent in the data ("calibration_check") and 2) specific messages to be integrated into the event data depending on "move_to_within
#'
#' @param ep.eye An initialized ep.eye object
#' @param inherit_btw_ev A named list taken from your config file. \code{inherit_btw_ev$calibration_check} can contain \code{$cal} and \code{$val} which are character vectors to search for in event .5 (prior to any recording event) and are checked to make sure GOOD is appended to these messages to ensure calibration and validation did not encounter any errors. The second element \code{inherit_btw_ev$move_to_within[[str/align_msg/pre_post]]} provides the opportunity to pass strings (\code{$str}) to search for amongst between-event messages, messages within events to align these messages to (\code{$align_msg}), and whether to assign between-event messages to the "pre" (e.g. moving message in 1.5 to 1) or "post" (e.g. moving message in 1.5 to 2) event (\code{$pre_post}). Elements of \code{$move_to_within} must be of the same length and will enounter error if this is not the case.
#' @param dt descriptive text to print to log file, defaults to NULL.
#'
#' @return ep.eye
#' @author Nate Hall
#'
#' @export
ep.eye_inherit_btw_ev <- function(ep.eye,
                                  inherit_btw_ev,
                                  dt = NULL){
  # browser()
  cat(dt)

  ### 2.12.1 Calibration/validation check
  if(!is.null(inherit_btw_ev$calibration_check)){
    cat("-- 2.12.1 Calibration/validation checks:\n")

    dt1 <- "--- 2.12.1.1 Calibration:"
    tryCatch.ep({
      c.check <- inherit_btw_ev$calibration_check$cal
      cal.msg <- ep.eye$metadata$btw_ev_msg %>% dplyr::filter(grepl(c.check, text, fixed = TRUE))
      if(!all(grepl("GOOD", cal.msg$text))){
        ep.eye$metadata$cal_check <- "warning"
        warning("cal check message does not contain GOOD", call. = FALSE)
      } else{
        ep.eye$metadata$cal_check <- "success"
      }
    },
    describe_text = dt1)

    dt2 <- "--- 2.12.1.2 Validation:"
    tryCatch.ep({
      v.check <- inherit_btw_ev$calibration_check$val
      val.msg <- ep.eye$metadata$btw_ev_msg %>% dplyr::filter(grepl(v.check, text, fixed = TRUE))
      if(!all(grepl("GOOD", val.msg$text))){
        ep.eye$metadata$val_check <- "warning"
        warning("val check message does not contain GOOD", call. = FALSE)
      } else{
        ep.eye$metadata$val_check <- "success"
      }
    },
    describe_text = dt2)
  } else{
    cat("-- 2.12.1 Calibration/validation checks: SKIP\n")
  }
  # browser()
  ### 2.12.2 Move requested messages to correct eventn depending on if it preceeds the event ("pre") or follows the event("post")
  if(!is.null(inherit_btw_ev$move_to_within)){
    dt3 <- "-- 2.12.2 Pull requested messages into measured data:"
    tryCatch.ep({
      mtw <- inherit_btw_ev$move_to_within
      stopifnot(all.equal(length(mtw$str), length(mtw$align_msg), length(mtw$pre_post)))
      for(m in 1:length(mtw$str)){
        ms <- mtw$str[m]

        # grab instances of string in between-event messages
        instances <- ep.eye$metadata$btw_ev_msg %>% dplyr::filter(grepl(ms, text)) %>%
          group_by(eventn) %>% mutate(eventn = ifelse(!eventn%%1==0, # update 11/12/20: only update eventn if value is not divisible by 0
                                                      ifelse(mtw$pre_post[m] == "post", # update event depending on pre-post designation
                                                             (eventn + .5),
                                                             eventn - .5),
                                                      eventn)) %>% data.table::data.table()

        # update 5/23/23: when no matching msgs will loop indefinitely. close this up by skipping if nrow = 0
        if(nrow(instances) == 0){
          warning(ms, " not found in any btwn_event_messages")
        } else {
          if(mtw$align_msg[m] == ""){
            ## no alignment message, pull into raw data in first or last "." position depending on pre_post.
            if(mtw$pre_post[m] == "post"){
              for(i in unique(instances$eventn)){
                first_dot_time <- ep.eye$raw %>% filter(eventn == i, et.msg == ".") %>% filter(time == min(time)) %>% pull(time)
                ep.eye$raw[which(ep.eye$raw$time == first_dot_time), "et.msg"] <- instances %>% filter(eventn == i) %>% pull(text)
              }
            } else{
              for(i in unique(instances$eventn)){
                last_dot_time <- ep.eye$raw %>% filter(eventn == i, et.msg == ".") %>% filter(time == max(time)) %>% pull(time)
                ep.eye$raw[which(ep.eye$raw$time == last_dot_time), "et.msg"] <- instances %>% filter(eventn == i) %>% pull(text)
              }
            }
          } else {
            raw_align <- ep.eye$raw %>% dplyr::filter(grepl(mtw$align_msg[m], ep.eye$raw$et.msg))
            # every instance of the message in question must have an alignment target.
            # stopifnot(nrow(raw_align) == nrow(instances))
            #
            # UPDATE 10/6/23: the above approach may be too strict. Really more, each instances just needs a joining entry in raw_align
            stopifnot(all(instances$eventn %in% raw_align$eventn))

            # recode time to immediately after alignment message (search "down" until reaching a point with no messages (coded as "."))
            for(i in 1:nrow(raw_align)){
              # print(i)
              # t1 <- as.numeric(raw_align[i, "time"])

              evn <- raw_align[i,"eventn"] %>% pull()

              df <- ep.eye$raw %>%
                dplyr::filter(eventn == evn)

              idx_end <- which(df$et.msg == mtw$align_msg[m])

              if(length(idx_end) > 0 && idx_end[length(idx_end)] != nrow(df)) {  # if "END_RECORDING" is not the last message
                t1_dot <- df$time[idx_end[length(idx_end)] + 1]  # take the time of the message after "END_RECORDING"
              } else if(length(idx_end) > 0 && idx_end[length(idx_end)] == nrow(df)) {  # if "END_RECORDING" is the last message
                t1_dot <- df$time[max(which(df$et.msg == "."))]  # take the time of the last "." message
              } else {
                t1_dot <- NA  # assign NA
                warning("Problem incorporating ")
              }

              instances[i, "time"] <- t1_dot
            }
            ep.eye$raw <- ep.eye$raw %>% left_join(instances, by = c("eventn", "time")) %>%  mutate(et.msg = ifelse(!is.na(text), text, et.msg)) %>% select(-text)
          }
        }
      }
      ep.eye$raw <- ep.eye$raw %>% select(-contains("btw_ev"))
    },
    describe_text = dt3)
  }
  return(ep.eye)
}
