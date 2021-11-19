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
#'
#' @return ep.eye Initialized ep.eye structure. [DETAILS HERE].
#' @author Nate Hall
#'
#' @importFrom data.table as.data.table data.table
#'
#' @export
ep.eye_setup_structure <- function(eye, task = NULL){
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

  # append edf filepath and task name to metadata
  ep.eye[["metadata"]][["edf_file"]] <- eye$edf_file
  if(!is.null(task)) ep.eye[["metadata"]][["task"]] <- task

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
                                     gaze_events = c("sacc", "fix", "blink"),
                                     confirm_correspondence = FALSE
                                     ){

  # Generate new columns to add gaze event information if requested.
  ep.eye$raw <- cbind(ep.eye$raw, setNames(data.frame(matrix(0, ncol = length(gaze_events), nrow = length(ep.eye$raw$time))), paste0(gaze_events, "n"))) #%>% tibble()

  issues <- list()
  substep <- 0
  for(i in gaze_events){
    substep <- substep + 1
    step <- paste0("2.6.", substep)
    cat("-- ",step, " ", i, ":\n", sep = "")
    step <- paste0(step, ".1")
    issues[[i]] <- list()

    # pull gaze event data
    gaze_event <- ep.eye$gaze[[i]]


    ## 6.i.1. event sequencing same between gaze metric and raw data? If not, this would mean that not a single gaze event happened during this trial, which could be a sign that something was amiss during this event (looking off screen/not paying attention).

    # Generate a flag in issue list that for these events there was no evidence of a certain event (not necessarily a sign of bad data).
    issues[[i]][["event_without_ev"]] <- which(!unique(ep.eye$raw$eventn) %in% unique(gaze_event$eventn))

    if(!all(unique(ep.eye$raw$eventn) %in% unique(gaze_event$eventn))){
      cat("--- ",step, " Search for events without gaze events: WARNING (",length(issues[[i]][["event_without_ev"]]),")\n", sep = "")
    } else{
      cat("---",step, " Search for events without gaze events: SUCCESS\n")
    }

    ### 8/4/21: set default to skip this section, it is quite a miniscule check and takes a long time to complete (can be up to 5-ish min per subject).
    ## 6.i.2. Two nit-picky checks: confirm timestamps are equal and present in raw and gaze_event data. confirm same event numbering between raw and gaze_event data. Then tag raw data with event number.
    # This essentially checks that correspondence between raw and extracted gaze events are exactly as expected.
    # in an ideal world these all run without issue, though even very minuscule mismatches will get flagged here. If there becomes some consistency in mismatches, perhaps it's worth doing some investigating.
    if(confirm_correspondence){

      step_26i2 <- paste0("2.6.", which(gaze_events == i), ".2")

      counts_26i2 <- list()#  "etime_mismatch" , "event_mismatch")
      # since this loops over typically thousands of gaze events, this is the most computationally intensive part of the initialization script.
      for (j in 1:nrow(gaze_event)) {
        # print(j)
        ev <- gaze_event[j,]
        etimes <- seq(ev$stime, ev$etime,1)
        ## pull from raw data
        r <- ep.eye$raw[ep.eye$raw$time %in% etimes,]

        # check 1: confirm timestamps are equal and present in raw and gaze_event data
        if(#!(length(r$time) == length(etimes)) | #if number of measurements dont match
          !all(r$time == etimes)    # this supersedes the above, forcing number of measurements to be exact and have timestamps be strictly equal
        ){
          counts_26i2[["etime_mismatch"]] <- c(counts_26i2[["etime_mismatch"]], j)
        }

        #check 2: confirm same event numbering between raw and gaze_event data.
        if(!ev$eventn == unique(r$eventn)){
          b_mismatch <- data.table("gaze_event" = i, "gaze_event_num" = j, "ev_event" = ev$eventn, "raw_num" = unique(r$eventn))
          counts_26i2[["event_mismatch"]] <- rbind(counts_26i2[["event_mismatch"]], b_mismatch)
        }

        #tag raw data with event number
        ep.eye$raw[which(ep.eye$raw$time %in% etimes), paste0(i,"n")] <- j
      }

      # all gaze_events should be represented in the raw data now +1 (0 represents no event)
      if(length(unique(as.matrix(ep.eye$raw[, paste0(i,"n")]))) != nrow(gaze_event) +1){
        counts_26i2[["raw_tag_gaze_events"]] <- length(unique(as.matrix(ep.eye$raw[, paste0(i,"n")])))
      }


      if(length(counts_26i2) != 0){
        issues[[i]][["raw_gaze_event_mismatches"]] <- counts_26i2
        cat("--- ",step_26i2, " Check timing mismatches between raw gaze data and extracted gaze events: WARNING (look in metadata for timing issues)\n", sep = "")
      } else { #perfect match, and all gaze_event tagging worked just fine.
        cat("--- ",step_26i2, " Check timing mismatches between raw gaze data and extracted gaze events: SUCCESS\n", sep = "")
      }

    }
  }

  ep.eye$metadata[["gaze_event_issues"]] <- issues

  ### Confirm that tagging raw data with GEV numbers successful
  tryCatch.ep({
    substep <- substep + 1
    gaze_event_tag_check <- 0

    stopifnot(all(paste0(gaze_events, "n") %in% names(ep.eye$raw)))
    for(i in gaze_events){
      # i <- "sacc"
      i.n <- paste0(i, "n")
      rawtag <- unique(ep.eye$raw[[i.n]])

      ## need to remove timestamps from raw data with no gaze_event (0) for proper matching
      rawtag<-rawtag[which(rawtag > 0)]

      # extract gaze_event numbers as they appear in the gaze field of the ep.eye list.
      cnum <- which(names(ep.eye$gaze[[i]]) == i.n)
      gaze_eventnums <- ep.eye$gaze[[i]][ ,..cnum] %>% as.matrix() %>% as.numeric()

      stopifnot(all(rawtag %in% gaze_eventnums))
    }
  }, describe_text = paste0("-- 2.6.", substep, " Confirm accurate tagging of raw data with gaze event numbers:"))

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
ep.eye_rm_crinfo <- function(ep.eye){
  cr <- unique(ep.eye$raw$cr.info)
  if(length(cr) == 1 & cr == "..."){
    ep.eye$raw <- ep.eye$raw %>% select(-cr.info)
  } else{
    stop(paste0("cr.info contains potentially important information (", paste0(cr, collapse = ","), ")"))
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
  ep.eye$raw <- ep.eye$raw %>%
    left_join(dplyr::filter(ep.eye$msg, btw_ev == 0), by = c("eventn", "time")) %>% dplyr::select(-btw_ev) %>%
    dplyr::rename(`et.msg` = `text`) %>%
    mutate(et.msg = ifelse(is.na(et.msg), ".", et.msg)) %>% data.frame()

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

  ### 3.10.1 meta_vars and vals
  dt1 <- "-- 3.10.1 Compare .edf info (session parameters) to expectations:"
  tryCatch.ep({

    stopifnot(!(is.null(meta_vars) | is.null(meta_vals))) # stop if either are null, need both.
    stopifnot(length(meta_vars) == length(meta_vals))


    meta_ref <- data.frame(meta_vars, meta_vals) %>% mutate_all(as.character)

    mismatch <- c() # append if any discrepancies.
    for(i in 1:nrow(meta_ref)){
      # message(eye$metadata[[meta_ref[i,"meta_vars"]]], " ", meta_ref[i,"meta_vals"])
      if(!ep.eye$metadata[[meta_ref[i,"meta_vars"]]] == meta_ref[i,"meta_vals"]){mismatch <- c(mismatch, i)}
    }

    if(!is.null(mismatch)){
      warning(mismatch,call. = FALSE)
      ep.eye[["metadata"]][["meta_check"]][["meta_vars_mismacth"]] <- mismatch
    } else {
      ep.eye[["metadata"]][["meta_check"]][["meta_vars_mismatch"]] <- 0
    }

  },
  describe_text = dt1)

  ### 3.10.2 confirm acceptable session length
  dt2 <- "-- 3.10.2 Compare recording time (session length) to expectations:"
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

  cat(dt)

  ### 3.12.1 Calibration/validation check
  if(!is.null(inherit_btw_ev$calibration_check)){
    cat("-- 3.12.1 Calibration/validation checks:\n")

    dt1 <- "--- 3.12.1.1 Calibration:"
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

    dt2 <- "--- 3.12.1.2 Validation:"
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
     cat("-- 3.12.1 Calibration/validation checks: SKIP\n")
  }
# browser()
  ### 3.12.2 Move requested messages to correct eventn depending on if it preceeds the event ("pre") or follows the event("post")
  if(!is.null(inherit_btw_ev$move_to_within)){
  dt3 <- "-- 3.12.2 Pull requested messages into measured data:"
    tryCatch.ep({
      mtw <- inherit_btw_ev$move_to_within
      stopifnot(all.equal(length(mtw$str), length(mtw$align_msg), length(mtw$pre_post)))
      for(m in 1:length(mtw$str)){
        ms <- mtw$str[m]

        # grab instances of string in between-event messages
        instances <- ep.eye$metadata$btw_ev_msg %>% dplyr::filter(grepl(ms, text)) %>%
          group_by(eventn) %>% mutate(eventn = ifelse(!eventn%%1==0, # update 11/12/20: only update eventn if value is not divisible by 0
                                                      ifelse(mtw$pre_post[m] == "pre", # update event depending on pre-post designation
                                                             (eventn + .5),
                                                             eventn - .5),
                                                      eventn)) %>% data.table()

        if(mtw$align_msg[m] == ""){
          ## no alignment message, pull into raw data in first or last "." position depending on pre_post.
          if(mtw$pre_post[m] == "pre"){
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
        } else{
          raw_align <- ep.eye$raw %>% dplyr::filter(grepl(mtw$align_msg[m], ep.eye$raw$et.msg))
          # every instance of the message in question must have an alignment target.
          stopifnot(nrow(raw_align) == nrow(instances))
          # recode time to immediately after alignment message (search "down" until reaching a point with no messages (coded as "."))
          for(i in 1:nrow(raw_align)){
            t1 <- as.numeric(raw_align[i, "time"])
            msg <- ep.eye$raw %>% dplyr::filter(time == t1) %>% select(et.msg) %>% as.character()
            while (msg != "."){ # search down until hitting first "."
              t1 <- t1 + 1
              msg <- ep.eye$raw %>% dplyr::filter(time == t1) %>% select(et.msg) %>% as.character()
            }
            instances[i,"time"] <- t1
          }
          ep.eye$raw <- ep.eye$raw %>% left_join(instances, by = c("eventn", "time")) %>%  mutate(et.msg = ifelse(!is.na(text), text, et.msg)) %>% select(-text)
        }
      }
      ep.eye$raw <- ep.eye$raw %>% select(-contains("btw_ev"))
    },
    describe_text = dt3)
  }
  return(ep.eye)
}
