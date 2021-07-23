#' Setup ep.eye structure

#' 
#' @param eye Loaded .edf file from \code{read_edf}
#' @param task Task name (character)
#' 
#' @return ep.eye Initialized ep.eye structure. [DETAILS HERE].
#' @author Nate Hall
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
               metadata = suppress_warnings(split(t(eye$info),f = colnames(eye$info)) %>% lapply(., function(x) {
                 if (x %in% c("TRUE", "FALSE")){
                   as.logical(x)} else if(!is.na(as.numeric(x))){
                     as.numeric(x)} else {x}
               } ))
  )

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

  # ### for large chunks of missing data, (e.g. if tracker turned off between trials), important to flag "recording chunks" of complete data
  # gen_run_num <- function(x){
  #   rl <- rle(is.na(x))
  #   lst <- split(x, rep(cumsum(c(TRUE, rl$values[-length(rl$values)])), rl$lengths))
  #   runvec <- c()
  #   for(i in 1:length(lst)){
  #     runvec <- c(runvec, rep(i, length(lst[[i]])))
  #   }
  #
  #   runvec <- ifelse(is.na(x), NA, runvec)
  #   return(runvec)
  # }
    return(ep.eye)
}

#' Unify gaze events with raw data
#' 
#' @description When loaded into the environment, an .edf file will be parsed with fields corresponding to $raw, $sacc, $fix, and $blink. Saccades, fixations, and blinks are called "gaze events" in the ep.eye nomenclature and denote the presence of an event of significance within gaze data. This function unifies raw data with gaze events by generating unique gaze event numbers, merges them to raw data and validates that timestamps from raw and gaze event fields are in correspondence.  
#' 
#' @param ep.eye An initialized ep.eye object
#' @param gaze_events Character vector of gaze_events to unify with raw. Defaults to unifying sacc, fix, and blink but can be set to any subset of these.
#' 
#' @return ep.eye ep.eye structure that has been tagged with gaze event numbers and validated for correspondence between raw and gaze event fields.
#' @author Nate Hall
#' 
#' @export
ep.eye_unify_gaze_events <- function(ep.eye, gaze_events = c("sacc", "fix", "blink")){

  # Generate new columns to add gaze event information if requested.
  ep.eye$raw <- cbind(ep.eye$raw, setNames(data.frame(matrix(0, ncol = length(gaze_events), nrow = length(ep.eye$raw$time))), paste0(gaze_events, "n"))) %>% tibble()

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
      cat("---",step, " Search for events without gaze events: COMPLETE\n")
    }


    ## 6.i.2. Two nit-picky checks: confirm timestamps are equal and present in raw and gaze_event data. confirm same event numbering between raw and gaze_event data. Then tag raw data with event number.
    # This essentially checks that correspondence between raw and extracted gaze events are exactly as expected.
    # in an ideal world these all run without issue, though even very minuscule mismatches will get flagged here. If there becomes some consistency in mismatches, perhaps it's worth doing some investigating.

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
      cat("--- ",step_26i2, " Check timing mismatches between raw gaze data and extracted gaze events: COMPLETE\n", sep = "")
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
  }, describe_text = paste0("- 2.7.", substep, " Confirm accurate tagging of raw data with gaze event numbers:"))
  
  return(ep.eye)
}






