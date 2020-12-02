#' generic function for initializing ep.eye object and performing basic internal checks on the eye data, while remaining agnostic to task/behavior structure.
#'
#' This includes validation of very basic data quality (large variance in gaze distribution, excessive blinks, large jumps in eye position, etc).
#' TODO: include functionality for logging of successes, warnings, failures. This will probably involve a trycatch statement that could handle a potentially large number of issues. We'll have to see how complicated it gets by balancing flexibility with parsimony. Tend to prefer flexibility if the package allows user-side functionality to be parsimonious :)
#' TODO: perhaps even store key variables (e.g. some measure of pupil fluctuation, or saccade velocity/acceleration) from prior subjects in separate circumscribed csv (which values get appended to) and plot distributions for every new subject. This would be akin to constructing a sort of empirical null distribution and performing informal (visual)"hypothesis tests" where we would hope certain variables in a given subject are not "significantly different" than the group distribution.
#' @param eye raw eye object pulled directly from the .edf file using read_edf(). Must be a list with expected_edf_fields c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file").


initialize_eye <- function(eye, config) {#, c. = 2) {

  if (class(eye) != "list") { stop("Something went wrong: initialize_eye requires list input.") }

  # cat("\n--------------\n", c., " Initialize eye object:\n--------------\n")
  cat("\n--------------\n2. Initialize eye object:\n--------------\n")

  ### 2.1 make sure all names are present
  expected_edf_fields <- c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file", "edf_file")
  stopifnot(all(expected_edf_fields %in% names(eye)))
  cat("- 2.1 Check expected fields: COMPLETE\n")

  ### 2.2 initialize basic eye object structure
  eout <- list(raw = eye$raw,
               gaze = list(sacc = as.data.table(eye$sacc) %>% mutate(saccn = 1:nrow(.)),
                           fix = as.data.table(eye$fix) %>% mutate(fixn = 1:nrow(.)),
                           blink = as.data.table(eye$blinks) %>% mutate(blinkn = 1:nrow(.))),
               pupil = list(fix = data.table(),
                            trial = data.table(),
                            event = data.table()),
               summary = list(counts = data.table(),
                              sacc = data.table(),
                              fix = data.table(),
                              blink = data.table(),
                              pupil = data.table()
               ),
               metadata = suppress_warnings(split(t(eye$info),f = colnames(eye$info)) %>% lapply(., function(x) {
                 if (x %in% c("TRUE", "FALSE")){
                   as.logical(x)} else if(!is.na(as.numeric(x))){
                     as.numeric(x)} else {x}
               } ))
  )

  eout[["metadata"]][["edf_file"]] <- eye$edf_file

  class(eout) <- c(class(eye), "ep.eye") #tag with ep.eye class

  cat("- 2.2 Initialize ep.eye list structure: COMPLETE\n")

  ### 2.3 document entire recording session length (if this is very different from BAU this should get flagged later)
  mintime <- min(eye$raw$time)
  maxtime <- max(eye$raw$time)
  all_time <- seq(mintime,maxtime,1)

  # store overall time for later
  tt <- maxtime-mintime
  eout$metadata$recording_time <- tt_sec <- tt/eout$metadata$sample.rate

  time_english <- lubridate::seconds_to_period(tt_sec) %>% as.character()
  cat("- 2.3 Document recording session length (", time_english,"): COMPLETE\n", sep = "")

  ### 2.4 check for continuity in timestamp on raw data

  # not sure what to do with the open gaps atm, but the fact that are for subsequent chunks of time makes me think this has to do with the structure fo the task rather than noise.

  if(all(all_time %in%  eye$raw$time)){
    missing_measurements <- 0 #if everything stricly accounted for (i.e. every sampling period from beginning to end has a measurement... in my experience this is unlikely, esp if between trials the eyetracker is told to stop/start sampling)
  } else{
    #store the gaps for later
    mm <- eye$raw$time[which(!all_time %in% eye$raw$time)] # missing measurements

    ###### deprecated: too cumbersome to store all missing measurements, and have instead elected to summarise in a compact DT below. Given how these missing events are generated, we know that they are consecutive events of missing data.
    # eout$metadata[["missing_measurements"]][["raw_events"]] <- mms <- split(mm, cumsum(c(1, diff(mm) != 1))) #contains all timestamps in between session start and end time that are missing blocked by consecutive timestamps. will likely want to dump before returning output
    # eout$metadata[["missing_measurements"]][["cumulative_byevent"]] <- lapply(mms, function(x) {length(x)}) %>% do.call(c,.) %>% as.numeric() # vector containing the length of each consecutive missing timestamp block.
    # eout$metadata[["missing_measurements"]][["summary"]] <-  data.table("start" = lapply(mms, function(x) {min(x)}) %>% do.call(c,.),
    #                                                                     "end" = lapply(mms, function(x) {max(x)}) %>% do.call(c,.),
    #                                                                     # "length" = eout$metadata[["missing_measurements"]][["cumulative_byevent"]])

    # much simplified.
    mms <- split(mm, cumsum(c(1, diff(mm) != 1))) # rather than exporting as metadata (too cumbersome) store for input into summary DT.
    mmls <- lapply(mms, function(x) {length(x)}) %>% do.call(c,.) %>% as.numeric() # same as above, just store in summary.
    eout$metadata[["missing_measurements"]] <-  data.table("start" = lapply(mms, function(x) {min(x)}) %>% do.call(c,.),
                                                           "end" = lapply(mms, function(x) {max(x)}) %>% do.call(c,.),
                                                           "length" = mmls)
    #abandon time_limits argument for now.
    # # convert to expected measurement range based on sampling rate
    # samp_range <- c(time_limits[1]*eout$metadata$sample.rate - time_limits[2]*eout$metadata$sample.rate,
    #                 time_limits[1]*eout$metadata$sample.rate + time_limits[2]*eout$metadata$sample.rate)
  }

  cat("- 2.4 Document missing measurements: COMPLETE\n")

  ### 5. check for continuity in events

  if(all(unique(eout$raw$eventn) == seq(min(unique(eout$raw$eventn)), max(unique(eout$raw$eventn)),1))){
    # confirmed that unique sorts in order they appear in the array. E.g. y <- c(1,1,3,3,2,3); unique(y) : [1] 1 3 2.
    # will therefor check for skipped events and the ordering.
    cat("- 2.5 Confirm raw event continuity: COMPLETE\n")
  } else{
    cat("- 2.5 Confirm raw event continuity: FAIL\n")
  }


  ### 6. check for matching between raw timestamps and saccades, fixations, blinks ("gaze events")

  gevs <- c("sacc", "fix", "blink")
  issues <- list()

  cat("- 2.6 Verify correspondence of gaze events and raw data:\n")

  # will end up tagging raw data with gev numbers
  eout$raw <- eout$raw %>% mutate(saccn = rep(0, length(eout$raw$time)),
                                  fixn = rep(0, length(eout$raw$time)),
                                  blinkn = rep(0, length(eout$raw$time)))

  for(i in gevs){
    step <- paste0("2.6.", which(gevs == i))
    cat("-- ",step, " ", i, ":\n", sep = "")
    step <- paste0(step, ".1")
    issues[[i]] <- list()

    # pull gaze event data
    gev <- eout$gaze[[i]]


    ## 6.i.1. event sequencing same between gaze metric and raw data? If not, this would mean that not a single gaze event happened during this trial, which could be a bit fishy.

    #may want to play with this later, but for now flag in list of issues that for these events there was no evidence of a certain event (not necessarily a sign of bad data)
    issues[[i]][["event_without_ev"]] <- which(!unique(eout$raw$eventn) %in% unique(gev$eventn))

    if(!all(unique(gev$eventn) == unique(eout$raw$eventn))){
      cat("--- ",step, " Search for events without gaze events: WARNING (",length(issues[[i]][["event_without_ev"]]),")\n", sep = "")
    } else{
      cat("---",step, " Search for events without gaze events: COMPLETE\n")
    }


    ## 6.i.2. Two nit-picky checks: confirm timestamps are equal and present in raw and gev data. confirm same event numbering between raw and gev data. Then tag raw data with event number.
    # This essentially checks that correspondence between raw and extracted gaze events are exactly as expected.
    # in an ideal world these all run without issue, though even very minuscule mismatches will get flagged here. If there becomes some consistency in mismatches, perhaps it's worth doing some investigating.

    step_26i2 <- paste0("2.6.", which(gevs == i), ".2")

    counts_26i2 <- list()#  "etime_mismatch" , "event_mismatch")
    # since this loops over typically thousands of gaze events, this is the most computationally intensive part of the initialization script.
    for (j in 1:nrow(gev)) {
      # print(j)
      ev <- gev[j,]
      etimes <- seq(ev$stime, ev$etime,1)
      ## pull from raw data
      r <- eout$raw[eout$raw$time %in% etimes,]

      # check 1: confirm timestamps are equal and present in raw and gev data
      if(#!(length(r$time) == length(etimes)) | #if number of measurements dont match
        !all(r$time == etimes)    # this supersedes the above, forcing number of measurements to be exact and have timestamps be strictly equal
      ){
        counts_26i2[["etime_mismatch"]] <- c(counts_26i2[["etime_mismatch"]], j)
      }

      #check 2: confirm same event numbering between raw and gev data.
      if(!ev$eventn == unique(r$eventn)){
        b_mismatch <- data.table("gev" = i, "gev_num" = j, "ev_event" = ev$eventn, "raw_num" = unique(r$eventn))
        counts_26i2[["event_mismatch"]] <- rbind(counts_26i2[["event_mismatch"]], b_mismatch)
      }

      #tag raw data with event number
      eout$raw[which(eout$raw$time %in% etimes), paste0(i,"n")] <- j
    }

    # all gevs should be represented in the raw data now +1 (0 represents no event)
    if(length(unique(as.matrix(eout$raw[, paste0(i,"n")]))) != nrow(gev) +1){
      counts_26i2[["raw_tag_gevs"]] <- length(unique(as.matrix(eout$raw[, paste0(i,"n")])))
    }


    if(length(counts_26i2) != 0){
      issues[[i]][["raw_gev_mismatches"]] <- counts_26i2
      cat("--- ",step_26i2, " Check timing mismatches between raw gaze data and extracted gaze events: WARNING (look in metadata for timing issues)\n", sep = "")
    } else { #perfect match, and all gev tagging worked just fine.
      cat("--- ",step_26i2, " Check timing mismatches between raw gaze data and extracted gaze events: COMPLETE\n", sep = "")
    }

  }

  eout$metadata[["gev_issues"]] <- issues

  # names(eout$metadata)
  # eout$metada[which(names(eout$metada) != "missing_measurements")]

  ### 7. Confirm that tagging raw data with GEV numbers successful

  gev_tag_check <- 0

  if (!all(paste0(gevs, "n") %in% names(eout$raw))) {
    gev_tag_check <- 1
  } else{ # meaning columns are existent
    for(i in gevs){
      # i <- "sacc"
      i.n <- paste0(i, "n")
      rawtag <- unique(eout$raw[[i.n]])

      ## need to remove timestamps from raw data with no gev (0) for proper matching
      rawtag<-rawtag[which(rawtag > 0)]

      # extract gev numbers as they appear in the gaze field of the ep.eye list.
      cnum <- which(names(eout$gaze[[i]]) == i.n)
      gevnums <- eout$gaze[[i]][ ,..cnum] %>% as.matrix() %>% as.numeric()

      if(!all(rawtag %in% gevnums)){gev_tag_check <- 2}
    }
  }

  if(gev_tag_check == 0){#success
    cat("- 2.7 Confirm accurate tagging of raw data with gaze event numbers: COMPLETE\n")
  } else{
    cat("- 2.7 Confirm accurate tagging of raw data with gaze event numbers: WARNING (",gev_tag_check,")\n", sep = "")
  }

  ### 8. Finish up tidying of raw DT: 1) store messages with no timestamp 2) verify uselessness of cr.info column 3) merge with messages, and "back-check" for errors.

  cat("- 2.8 Finish raw DT tidying:\n", sep = "")

  # 8.1 store messages with no timestamp match in raw data (collected between trials with no corresponding measurements).
  # In my (neighborhood) checks these have to do with calibration parameters, display coords, etc. For most users this will not be very helpful.
  # N.B. however, if the user passes important information before turning the tracker on (as in the sorting mushrooms data), it will be important to allow for users to move messages in the interstitial spaces between recordings to the beginning of a trial/event. Later will include this in the YAML parsing framework.
  btw_tr <- eye$msg %>% anti_join(eout$raw, by = "time") %>% data.table()
  if(nrow(btw_tr) == 0){
    cat("-- 2.8.1 Between-trial message storage: COMPLETE (EMPTY)\n", sep = "")
  } else{
    eout$metadata[["btw_tr_msg"]] <- btw_tr
    cat("-- 2.8.1 Between-trial message storage: COMPLETE (NON-EMPTY)\n", sep = "")
  }

  # 8.2 drop cr.info column
  cr <- unique(eout$raw$cr.info)
  if(length(cr) == 1 & cr == "..."){
    eout$raw <- eout$raw %>% select(-cr.info)
    cat("-- 2.8.2 Drop cr.info in raw data: COMPLETE\n", sep = "")
  } else{ cat("-- 2.8.2 Retain cr.info in raw data: COMPLETE (",paste0(cr, collapse = ","),")\n", sep = "")}

  # 8.3 merge messages to raw data.
  # N.B. the left_join means that between trial messages will not be copied over but rather are stored in metadata if between trial messages are of interest. Since there is no corresponding measurements of gaze/pupil in the raw data there is nowhere in the raw time series to place these relatively unhelpful messages.
  # N.B. Under the current set-up this operation will increase the number of rows if multiple messages are passed simultaneously. At a later point, one could change this format with the yaml config file under $definitions$eye$coincident_msg.
  eout$raw <- eout$raw %>% left_join( dplyr::filter(eye$msg, !text %in% unique(btw_tr$text)), by = c("eventn", "time")) %>% rename(`et.msg` = `text`)  %>% mutate(et.msg = ifelse(is.na(et.msg), ".", et.msg)) %>% data.table()

  # important to back-translate to original messages due to the use of left_join.
  umsg <- unique(eout$raw$et.msg)[which(unique(eout$raw$et.msg) != ".")] #unique messages in the final output.

  if(nrow(btw_tr) != 0){
    umsg_edf <- unique(eye$msg$text) # unadulterated, right off the eyetracker.
    umsg_orig <- umsg_edf[which(!umsg_edf %in% unique(btw_tr$text))] # make sure to eliminate between-trial messages and just grab messages that are passed while recording.

    # length(umsg) + length(unique(btw_tr$text)) == length(unique(eye$msg$text))
  } else{
    umsg_orig <- eye$msg$text # no btwn-trial messages to filter out.
  }

  if(all(umsg %in% umsg_orig)){
    cat("-- 2.8.3 Merge raw gaze data with eyetracker messages, with successful back-translate: COMPLETE\n", sep = "")
  } else{ # if any mismatch between what is contained in raw data and original message structure, print error and do some digging.

    miss_msgs <- umsg[!umsg %in% umsg_orig]
    eout$metadata[["missing_messages_raw"]] <- miss_msgs

    mmsgs_stamped <- eye$msg[which(eye$msg$text %in% miss_msgs), ]
    for(i in 1:nrow(mmsgs_stamped)){
      mstamp <- mmsgs_stamped[i,2] #grab missing timestamp.
      mstamp %in% mm
      mm
    }


    cat("-- 2.8.3 Merge raw gaze data with eyetracker messages, with successful back-translate: WARNING: errors in this step have not been fully vetted. \n", sep = "")
  }

  cat("\n")
  return(eout)
}





#' Tidy timeseries
#'

tidy_eye_timeseries <- function(eye, config, header = "4. Tidy raw timeseries:"){

  log_chunk_header(header)

  ### 4.1 Extract timeseries configuration options
  tryCatch.ep({
    c.ts <- tidy_eye_config(config)[["ts"]]
    stopifnot(all(c("downsample_bins") %in% names(c.ts)))
  }, describe_text = "- 4.1 Extract TS config options:")

  ### 4.2 Reset timestamps to 0 at first recording
  tryCatch.ep({
    t_start <- eye$raw$time[1]
    eye$metadata$t_start <- t_start

    # raw
    eye$raw$time <- eye$raw$time - t_start
    # saccades
    eye$gaze$sacc$stime <- eye$gaze$sacc$stime - t_start
    eye$gaze$sacc$etime <- eye$gaze$sacc$etime - t_start
    # fixations
    eye$gaze$fix$stime <- eye$gaze$fix$stime - t_start
    eye$gaze$fix$etime <- eye$gaze$fix$etime - t_start
    # blinks
    eye$gaze$blink$stime <- eye$gaze$blink$stime - t_start
    eye$gaze$blink$etime <- eye$gaze$blink$etime - t_start
    #meta-data
    eye$metadata$missing_measurements$start <- eye$metadata$missing_measurements$start - t_start
    eye$metadata$missing_measurements$end <- eye$metadata$missing_measurements$end - t_start
    eye$metadata$btw_tr_msg$time <- eye$metadata$btw_tr_msg$time - t_start



  }, describe_text = "- 4.2 Shift timestamps to 0 start point:")


  ### 4.3 Downsample gaze
  dt <- "- 4.3 Downsample Gaze:\n"
  if("gaze" %in% names(c.ts$downsample_bins)){
    # eye <-  handle_between_trial(c.e, eye, dt)
    tryCatch.ep({
      ds <- downsample_gaze(eye$raw, bin.length = c.ts$downsample_bins$gaze, aggvars = c("eventn", "event", "block", "block_trial"))


    },describe_text = dt)

    down

  } else{
    cat(paste0(dt, " SKIP\n"))
  }

}





downsample_eye <- function(eye, downsample_factor=1, method = "subsample_avg"){#digital_channels=c("ttl_code", "ttl_onset", "Digital.*"), method="subsample") {
  stopifnot(inherits(eye, "ep.eye"))
  # if (is.null(acq_data$ttl_codes)) { stop("Cannot find $ttl_codes element in ep.physio object. Run augment_ttl_details?") }
  if (is.null(eye$raw)) { stop("Cannot find $raw element in ep.eye object") }
  assert_data_table(eye$raw) #for now, we are using data.table objects, so DT syntax applies
  assert_count(downsample_factor)

  orig_cols <- names(eye$raw)
  t_cols <- "time" #hard code single time column for now
  eye_cols <- orig_cols[!orig_cols %in% t_cols]

  # d_cols <- grep(paste0("^(", paste(digital_channels, collapse="|"), ")$"), phys_cols, perl=TRUE, value=TRUE)
  # a_cols <- phys_cols[!phys_cols %in% d_cols]
  a_cols <- eye_cols

  #time downsampling should use the subsampling approach since it is not a periodic signal
  if (length(t_cols) > 0L) {
    time_data <- lapply(eye$raw[, ..t_cols], function(col) { col[seq(1, length(col), downsample_factor)] })
  } else {
    time_data <- NULL
  }

  if (length(a_cols) > 0L) {
    if (method=="decimate") {
      analog_data <- lapply(eye$raw[, ..a_cols], function(col) {
        #calculation channels that involve filtering can have trailing zeros that throw off decimate
        nas = which(is.na(col))
        if (length(nas) > 0L) {
          if (all(diff(nas) == 1) && max(nas) == length(col)) { #only works for trailing NAs
            message("Replacing ", length(nas), " trailing NAs with zeros to permit decimation. Check trailing elements if they are important.")
            col[nas] <- 0 #replace trailing NAs with zero
            #col <- col[1:(nas[1]-1)] #all elements before the first NA
            #navec <- rep(NA_real_, ceiling(length(col)/downsample_factor))
          } else { stop("NAs present in signal that are not at the end. Cannot decimate.") }
        }

        padl <- plyr::round_any(.1*length(col), downsample_factor)
        cpad <- c(rep(0, padl), col, rep(0, padl))
        nz_dsamp <- padl/downsample_factor
        dsig <- decimate(cpad, q=downsample_factor, ftype="iir")
        return(dsig[(nz_dsamp+1):(length(dsig)-nz_dsamp)])
      })
    } else if (method=="subsample") {
      analog_data <- lapply(eye$raw[, ..a_cols], function(col) {col[seq(1, length(col), downsample_factor)] })
    } else if(method == "subsample_avg"){
      # analog_data <- lapply(eye$raw[, ..a_cols], function(col) {
      #
        subsamps <- data.frame(start = seq(1, nrow(eye$raw), downsample_factor)) %>%
          mutate(end = start + (downsample_factor -1)) %>% rownames_to_column() %>% rename(`time_ds` = `rowname`) %>% mutate(time_ds = as.numeric((time_ds)))

        # object.size(eye)/1000000
        downsamps_raw <- list()
        # library(tictoc)
        tic()
        for(ds in subsamps$time_ds){
          downsamps_raw[[ds]] <- ss <- eye$raw[subsamps[which(subsamps$time_ds == ds),"start"]:subsamps[which(subsamps$time_ds == ds),"end"],]
        }
        toc()
        analog_data <- lapply(subsamps[1:10,"time_ds"], function(tds) {

        })


        x <- sampstarts %>% group_by(time_ds) %>% dplyr::mutate(time_ds_mean = median(col[start:end], na.rm = TRUE))
        print(x, n =500)
        # })
    } else { stop("unknown downsampling method: ", method) }
  } else {
    analog_data <- NULL
  }

  # if (length(d_cols) > 0L) {
  #   #could support subsampling here -- doesn't seem like a great idea, though
  #   digital_data <- lapply(eye$raw[, ..d_cols], function(col) { downsample_digital_timeseries(col, downsample_factor, TRUE) })
  # } else {
  #   digital_data <- NULL
  # }

  if (is.null(analog_data)) {
    ret <- digital_data
  } else if (is.null(digital_data)) {
    ret <- analog_data
  } else {
    ret <- cbind(as.data.frame(analog_data), as.data.frame(digital_data))
    ret <- ret[,phys_cols] #revert to original column order
  }

  if (!is.null(time_data)) { ret <- cbind(ret, as.data.frame(time_data)) }

  eye$raw <- ret[,orig_cols] #put back in original column order
  attr(ret, "sampling_rate") <- attr(ret, "sampling_rate")/downsample_factor
  attr(ret, "max_channel_rate") <- attr(ret, "max_channel_rate")/downsample_factor
  eye$sampling_rate <- eye$sampling_rate/downsample_factor
  eye$max_channel_rate <- eye$max_channel_rate/downsample_factor

  #downsample onsets in $ttl_codes
  if (!is.null(eye$ttl_codes)) {
    eye$ttl_codes$onset <- floor(eye$ttl_codes$onset/downsample_factor) #round toward earlier samples
    eye$ttl_codes$offset <- floor(eye$ttl_codes$offset/downsample_factor) #round toward earlier samples
  }

  return(eye)

}





downsample_gaze <- function(dataframe, bin.length = 50, timevar = "time", aggvars = c("subject", "condition", "target", "trial", "object", "timebins"), type="gaze"){
  if(type=="gaze") {
    browser()
    downsample <- dataframe %>%
      mutate(timebins = round(!!sym(timevar)/bin.length)*bin.length)
    # if there are aggregation variables, then aggregate
    if(length(aggvars > 0)){
      downsample <- downsample %>%
        dplyr::group_by_(.dots = aggvars) %>%
        dplyr::summarize(acc = unique(acc), rt = unique(rt),
                         Fix = mean(Fix) > 0.5) %>%
        dplyr::ungroup()
    }
    return(downsample)
  }
  # if (type=="pupil") {
  #   downsample <- dataframe %>%
  #     mutate(timebins = round(!!sym(timevar)/bin.length)*bin.length)
  #   # if there are aggregation variables, then aggregate
  #   if(length(aggvars > 0)){
  #     downsample <- downsample %>%
  #       dplyr::group_by(.dots = aggvars) %>%
  #       dplyr::summarize(aggbaseline=mean(baselinecorrectedp)) %>%
  #       dplyr::ungroup()
  #   }
  # }
  return(downsample)
}










