#' general wrapper for reading eye data into the package and performing all QA and processing
read_process_eye <- function(file, gen_log = TRUE, log_dir = NULL,  parser=NULL, ...) {

  stopifnot(file.exists(file))
  if (length(file) > 1L) { stop("At present, read_process_eye is designed for one file at a time") }

  # initialize log file if requested. Otherwise will print feedback while running checks.
  ## N.B. right now this will overwrite existing files, can come back to later.
  if(gen_log){init_eyelog(log_dir)}

  ### 1. Read EDF file
  eye <- read_edf(file, keep_asc=FALSE, parse_all=TRUE)[[1]] # samples=FALSE removes read-in of raw data (significantly speeding up computation speed), but we'll keep this for now.


  ### 2. Perform basic data reorganization/wrangling validation checks and compute new variables
  eye2 <- wrangle_validate_eye(eye)

  ### 3. Split pupil into separate structure


  ### Parse eye messages from experiment.pipeline YAML

  if (is.null(parser)) {
    # stop("Need to pass parser function to read_eye")
    message("Only generic read and validation applied")
  } else{
    message("eye message parsing applied via: ", parser)
    eye <- parser(eye, ...)
  }

  # class(eye) <- c(class(eye), "ep.eye") #tag with ep.eye class

  #other general post-processing for eye data
  return(eye)
}



#' generic function for doing initial test on eye data.
#'
#' This includes validation of very basic data quality (large variance in gaze distribution, excessive blinks, large jumps in eye position, etc).
#' TODO: include functionality for logging of successes, warnings, failures. This will probably involve a trycatch statement that could handle a potentially large number of issues. We'll have to see how complicated it gets by balancing flexibility with parsimony. Tend to prefer flexibility if the package allows user-side functionality to be parsimonious :)
#' TODO: perhaps even store key variables (e.g. some measure of pupil fluctuation, or saccade velocity/acceleration) from prior subjects in separate circumscribed csv (which values get appended to) and plot distributions for every new subject. This would be akin to constructing a sort of empirical null distribution and performing informal (visual)"hypothesis tests" where we would hope certain variables in a given subject are not "significantly different" than the group distribution.


wrangle_validate_eye <- function(eye # raw imported .edf file
                                 # note to self: why dont we just always print real time of task to log file, add to metadata, and call on this when doing QA checks?
                                 # time_limits = NULL  # can be null or expected time limits for the task: c(mean, margin of error above AND below) in SECONDS. If not null, time_limits = c(1800, 900) would mean that one expects the task to be about 30 min, but would not be concerned if the task ran 15 min short or long. Regardless, will print task time
) {

  if (class(eye) != "list") { stop("Something went wrong: wrangle_validate_eye requires list input.") }



  ### 2.1 make sure all names are present
  expected_edf_fields <- c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file")
  stopifnot(all(expected_edf_fields %in% names(eye)))
  cat("2.1 Check expected fields: COMPLETE")

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

  eout[["metadata"]][["edf_file"]] <- edf_path

  class(eout) <- c(class(eye), "ep.eye") #tag with ep.eye class

  cat("2.2 Initialize ep.eye list structure: COMPLETE")

  ### 2.3 document entire recording session length (if this is very different from BAU this should get flagged later)
  mintime <- min(eye$raw$time)
  maxtime <- max(eye$raw$time)
  all_time <- seq(mintime,maxtime,1)

  # store overall time for later
  tt <- maxtime-mintime
  eout$metadata$recording_time <- tt_sec <- tt/eout$metadata$sample.rate

  time_english <- lubridate::seconds_to_period(tt_sec) %>% as.character()
  cat("\n2.3 Document recording session length (", time_english,"): COMPLETE")

  ### 2.4 check for continuity in timestamp on raw data

  # not sure what to do with the open gaps atm, but the fact that are for subsequent chunks of time makes me think this has to do with the structure fo the task rather than noise.

  if(all(all_time %in%  eye$raw$time)){
    missing_measurements <- 0 #if everything stricly accounted for (i.e. every sampling period from beginning to end has a measurement)
  } else{
    #store the gaps for later
    mm <- eye$raw$time[which(!all_time %in% eye$raw$time)]

    eout$metadata[["missing_measurements"]][["raw_blocks"]] <- split(mm, cumsum(c(1, diff(mm) != 1))) # will likely want to dump before returning output
    eout$metadata[["missing_measurements"]][["cumulative_byblock"]] <- lapply(mms, function(x) {length(x)}) %>% do.call(c,.) %>% as.numeric()

    #abandon time_limits argument for now.
    # # convert to expected measurement range based on sampling rate
    # samp_range <- c(time_limits[1]*eout$metadata$sample.rate - time_limits[2]*eout$metadata$sample.rate,
    #                 time_limits[1]*eout$metadata$sample.rate + time_limits[2]*eout$metadata$sample.rate)
  }

  cat("\n2.4 Document missing measurements: COMPLETE")

  ### 5. check for continuity in blocks

  if(all(unique(eout$raw$block) == seq(min(unique(eout$raw$block)), max(unique(eout$raw$block)),1))){
    # confirmed that unique sorts in order they appear in the array. E.g. y <- c(1,1,3,3,2,3); unique(y) : [1] 1 3 2.
    # will therefor check for skipped blocks and the ordering.
      cat("\n2.5 Confirm raw block continuity: COMPLETE")
  } else{
    cat("\n2.5 Confirm raw block continuity: FAIL")
  }


  ### 6. check for matching between raw timestamps and saccades, fixations, blinks ("gaze events")

  gevs <- c("sacc", "fix", "blink")
  issues <- list()

  for(i in gevs){
    step <- paste0("2.6.", which(gevs == i))
    issues[[i]] <- list()

    # pull gaze event data
    gev <- eout$gaze[[i]]


    ## 1. block sequencing same between gaze metric and raw data? If not, this would mean that not a single gaze event happened during this trial, which could be a bit fishy.
    if(!all(unique(gev$block) == unique(eout$raw$block))){
      cat("\n",step, " Confirm block continuity with raw data for", i,": FAIL")
    } else{
      cat("\n",step, " Confirm block continuity with raw data for", i,": COMPLETE")
    }

    #may want to play with this later, but for now flag in list of issues that for these blocks there was no evidence of a certain event (not necessarily a sign of bad data)
    issues[[i]][["block_without_ev"]] <- which(!unique(eout$raw$block) %in% unique(gev$block))

    ## 2. regardless of if they match, confirm timestamps are paired with same block numbering

    for (j in 1:nrow(gev)) {
        ev <- gev[j,]
        etimes <- seq(ev$stime, ev$etime,1)
        ## pull from raw data
        r <- eout$raw[eout$raw$time %in% etimes,]
        unique(r$block)
    }





  }
  eout$gaze$


  ### 7. merge raw, sacc, fixations, blinks


  ### 6. check for NAs in raw data



  return(eye_parsed)
}


#'
#'
#'






#' specific function for importing neighborhood eye tracking data into the package
#' need to add task-specific processing
#' @export
read_eye_neighborhood <- function(file) {
  if (length(file) > 1L) { stop("At present, read_eye_neighborhood is designed for one file at a time") }
  #store as temp asc file
  eye_parsed <- read_edf(file, keep_asc=FALSE, parse_all=TRUE)[[1]] #read_edf always returns list -- here we only one the one file
  return(eye_parsed)
}



