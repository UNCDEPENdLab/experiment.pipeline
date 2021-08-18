############################
##### List of subsidiary functions utilized in `ep.eye_preprocess_gaze()` and `ep.eye_preprocess_pupil()`
############################
##### Gaze
# - ep.eye_rm_impossible()
# - ep.eye_add_aois()
# -- ep.eye_gen_aoi_ref()
# -- ep.eye_gen_aoi_look()
# - ep.eye_collapse_time()
# - ep.eye_downsample()
# -- subsample_dt()
# -- downsample_chars()
##### Pupil
# - ep.eye_extend_blinks()
# - ep.eye_smooth_pupil()
# -- movavg.ep()
# - ep.eye_interp_pupil()
# - ep.eye_baseline_correct()
############################

#' Remove values outside of screen dimensions
#'
#' @param ep.eye an ep.eye object
#' @param dt descriptive text to print to log file, defaults to NULL.
#'
#' @export

ep.eye_rm_impossible <- function(ep.eye, dt = NULL){
  tryCatch.ep({
    ep.eye$raw <- ep.eye$raw %>% mutate(xp = ifelse(xp >= ep.eye$metadata$screen.x | xp <= 0, NA, xp),
                                                        yp = ifelse(yp >= ep.eye$metadata$screen.y | yp <= 0, NA, yp),
                                                        xp = ifelse(is.na(yp), NA, xp),
                                                        yp = ifelse(is.na(xp), NA, yp),
  )
  }, describe_text = dt)
  return(ep.eye)
}

#' Add AOI information to gaze data
#'
#' @param ep.eye an ep.eye object
#' @param indicator a regex containing string to search for across messages for AOI information
#' @param extract_coords a regex for extracting AOI coordinates
#' @param extract_labs a regex for extracting AOI labels
#' @param split_coords string denoting what separator is being used to split AOI coordinates
#' @param tag_raw logical. tag raw data with AOI specific information? Default to FALSE.
#'
#' @export

ep.eye_add_aois <- function(ep.eye,
                            indicator,
                            extract_coords,
                            extract_labs,
                            split_coords,
                            tag_raw = FALSE
                            ){
  ### 4.1.1 pull AOI information into new columns by eventn
  aoi_ref <- ep.eye_gen_aoi_ref(ep.eye,
                         indicator,
                         extract_coords,
                         extract_labs,
                         split_coords,
                         dt = "-- 4.2.1 Generate AOI reference object  (note. currently only regex supported for rectangular AOIs):")

  ### 4.1.2 tag gaze data with AOI_look field
  ep.eye <- ep.eye_gen_aoi_look(ep.eye,
                         aoi_ref,
                         tag_raw,
                         dt = "-- 4.2.2 Generate AOI fields in data:")

  return(ep.eye)
}

#' Generate AOI-reference object
#'
#' @param ep.eye an ep.eye object
#' @param indicator a regex containing string to search for across messages for AOI information
#' @param extract_coords a regex for extracting AOI coordinates
#' @param extract_labs a regex for extracting AOI labels
#' @param split_coords string denoting what separator is being used to split AOI coordinates
#'
#' @export

ep.eye_gen_aoi_ref <- function(ep.eye,
                               indicator,
                               extract_coords,
                               extract_labs,
                               split_coords,
                               dt){
  tryCatch.ep({

    #append to this df and write to metadata for reference of aoi labels and coordinates per eventn
    aoi_ref <- data.frame()

    for(i in unique(ep.eye$raw$eventn)){
      ev <- ep.eye$raw %>% dplyr::filter(eventn == i)
      aois <- ev$et.msg[grepl(indicator ,ev$et.msg)]

      # N.B. currently aoi coords are tagged as x1, y1, x2, y2 according to the extract_coords specification. This is not very flexible and will want to revisit this for sure.
      for (a in aois) {
        coords <- as.numeric(do.call(c, str_split(str_extract(a, extract_coords), split_coords)))
        lab <- str_extract(a, extract_labs)
        aoi_ref <- rbind(aoi_ref, data.frame(eventn = i,
                                             aoi_msg = a,
                                             x1 = coords[1], y1 = coords[2], x2 = coords[3], y2 = coords[4],
                                             aoi_lab = lab))
      }
    }
  }, describe_text = dt)

  aoi_ref$aoi_lab <- as.character(aoi_ref$aoi_lab)
  return(aoi_ref)
}

#' Attaches AOI information to gaze data
#'
#' @param ep.eye an ep.eye object
#' @param aoi_ref data.frame containing AOI information
#' @param tag_raw logical. tag raw data with AOI specific information? Default to FALSE.
#' @param dt descriptive text to print to log file, defaults to NULL.
#'
#' @export

ep.eye_gen_aoi_look <- function(ep.eye, aoi_ref, tag_raw = FALSE, dt = NULL){
  # TODO allow for user to pass their own AOI_ref object
  cat(dt, "\n")
  if(tag_raw){ ## leaving in as an option, though I think it is probably more important to gauge which AOIs were the focus during saccades (to/from) and fixations.
    dt <- "--- 4.2.2.0 Appending AOIs to raw data"
    tryCatch.ep({
      ep.eye$raw$aoi_look <- "."
      for(i in 1:nrow(ep.eye$raw)) {
        print(i)
        evn <- ep.eye$raw[[i,"eventn"]]
        ev_aois <- aoi_ref %>% dplyr::filter(eventn == evn) %>% group_by(aoi_lab) %>% mutate(aoi_look = ifelse(ep.eye$raw[[i,"xp"]] >= min(x1,x2) & ep.eye$raw[[i,"xp"]] <= max(x1,x2) &
                                                                                           ep.eye$raw[[i,"yp"]] >= min(y1,y2) & ep.eye$raw[[i,"yp"]] <= max(y1,y2), TRUE, FALSE)) %>% ungroup()

        if(any(ev_aois$aoi_look)){
          ep.eye$raw[i,"aoi_look"] <- ev_aois %>% dplyr::filter(aoi_look) %>% pull(aoi_lab) %>% paste(collapse = "/") # if gaze falls within 2 aoi rects, will collapse into single aoi_look variable, separated by /. E.g. face/eyes, aoi1/aoi2.
        }
      }
    },describe_text = dt)
  }

  # saccades
  dt <- "--- 4.2.2.1 Appending AOIs to saccade data"
  tryCatch.ep({
    ep.eye$gaze$sacc$aoi_start <- "."
    ep.eye$gaze$sacc$aoi_end <- "."
    for (i in 1:nrow(ep.eye$gaze$sacc)) {
      # print(i)
      i <- 1

      evn <- ep.eye$gaze$sacc[[i,"eventn"]]

      ev_aois <- aoi_ref %>% dplyr::filter(eventn == evn) %>% group_by(aoi_lab) %>% mutate(aoi_start = ifelse(ep.eye$gaze$sacc[[i,"sxp"]] >= min(x1,x2) & ep.eye$gaze$sacc[[i,"sxp"]] <= max(x1,x2) &
                                                                                                                ep.eye$gaze$sacc[[i,"syp"]] >= min(y1,y2) & ep.eye$gaze$sacc[[i,"syp"]] <= max(y1,y2), TRUE, FALSE),
                                                                                            aoi_end = ifelse(ep.eye$gaze$sacc[[i,"exp"]] >= min(x1,x2) & ep.eye$gaze$sacc[[i,"exp"]] <= max(x1,x2) &
                                                                                                              ep.eye$gaze$sacc[[i,"eyp"]] >= min(y1,y2) & ep.eye$gaze$sacc[[i,"eyp"]] <= max(y1,y2), TRUE, FALSE)) %>% ungroup()

      # NAs denote missing measurements at the beginning or end of saccade. These may need to be dumped ultimately. For now, it's easiest to code them as "no aoi" and let later scripts handle this.
      if(all(is.na(ev_aois$aoi_start))) {ev_aois$aoi_start <- FALSE}
      if(all(is.na(ev_aois$aoi_end))) {ev_aois$aoi_end <- FALSE}

      if(any(ev_aois$aoi_start)){
        ep.eye$gaze$sacc[i,"aoi_start"] <- ev_aois %>% dplyr::filter(aoi_start) %>% pull(aoi_lab) %>% paste(collapse = "/") # if gaze falls within 2 aoi rects, will collapse into single aoi_look variable, separated by /. E.g. face/eyes, aoi1/aoi2.
      }

      if(any(ev_aois$aoi_end)){
        ep.eye$gaze$sacc[i,"aoi_end"] <- ev_aois %>% dplyr::filter(aoi_end) %>% pull(aoi_lab) %>% paste(collapse = "/")
      }

    }
  }, describe_text = dt)


  # fixations
  dt <- "--- 4.2.2.2 Appending AOIs to fixation data"
  tryCatch.ep({
    ep.eye$gaze$fix$aoi_look <- "."
    for (i in 1:nrow(ep.eye$gaze$fix)) {
      # print(i)
      # i <- 1

      evn <- ep.eye$gaze$fix[[i,"eventn"]]

      ev_aois <- aoi_ref %>% dplyr::filter(eventn == evn) %>% group_by(aoi_lab) %>% mutate(aoi_look = ifelse(ep.eye$gaze$fix[[i,"axp"]] >= min(x1,x2) & ep.eye$gaze$fix[[i,"axp"]] <= max(x1,x2) &
                                                                                          ep.eye$gaze$fix[[i,"ayp"]] >= min(y1,y2) & ep.eye$gaze$fix[[i,"ayp"]] <= max(y1,y2), TRUE, FALSE)) %>% ungroup()

      # NAs denote missing measurements
      if(all(is.na(ev_aois$aoi_look))) {ev_aois$aoi_look <- FALSE}


      if(any(ev_aois$aoi_look)){
        ep.eye$gaze$fix[i,"aoi_look"] <- ev_aois %>% dplyr::filter(aoi_look) %>% pull(aoi_lab) %>% paste(collapse = "/") # if gaze falls within 2 aoi rects, will collapse into single aoi_look variable, separated by /. E.g. face/eyes, aoi1/aoi2.
      }


    }}, describe_text = dt)

  ep.eye$metadata$aoi_ref <- data.table(aoi_ref)
  return(ep.eye)
}

#' Collapse timing to one row per time (paste multiple messages in a single et.msg)
#' @param ep.eye an ep.eye object
#' @param dt descriptive text to print to log file, defaults to NULL.
#'
ep.eye_collapse_time <- function(ep.eye, dt){
  tryCatch.ep({
    ep.eye$raw <- ep.eye$raw %>% data.frame() %>%
      dplyr::group_by(eventn, time, xp, yp, ps, saccn, fixn, blinkn, block, block_trial, event) %>%
      dplyr::summarise(et.msg = paste(et.msg, collapse = " | "), .groups = "drop") %>% data.table()
    # browser()

    if(unique(table(ep.eye$raw$time)) != 1){
      warning("Some measurements have more than one row")
    }
  },
  describe_text = dt
  )
  return(ep.eye)
}

#' Downsample gaze and/or pupil data
#'
#' @param df data.table to downsample (usually this is ep.eye$raw)
#' @param downsample_factor Numeric value indicating degree of downsampling. A factor of 50, collapses 50 measurements into 1, so it is important to be mindful of your sampling rate when selecting this value. Defaults to 50 (which moves a second of recording at 1000Hz to 20 measurements).
#' @param digital_channels Character vector of columns in df that are integer values that should not be combined but blocked by.
#' @param analog_channels Character vector of columns in df that are continually varying and should be summarized within the downsampling procdure (incl x and y gaze position and pupil size)
#' @param char_channels Character vector of columns in df that represent values that should be combined if there are unique values within a downsampling block. If there are multiples within a downsampling block will paste them together with " || "
#' @param method String "mean" or "subsample" determining whether to perform subsampling (keep every n measurement) or mean-based downsampling.
#'
#' @export
ep.eye_downsample <- function(df,
                              downsample_factor=50,
                              digital_channels = c("eventn", "saccn", "fixn", "blinkn", "block_trial"),
                              analog_channels = c("xp", "yp", "ps"),
                              char_channels = c("et.msg", "block", "event"),
                              # TODO could be useful to implement so chunks with large amt of  missing measurements get dropped/set to NA. min_samps = 10, #
                              method = "mean") {
  ### debug
  # df <- ep.eye$raw
  # downsample_factor = downsample$factor
  # digital_channels = c("eventn", "saccn", "fixn", "blinkn", "block_trial") # integer values of trial, event, and gevs.
  # analog_channels = c("xp", "yp") # gaze and pupil measurements
  # char_channels = c("et.msg", "block", "event") # characters, if there are unique values within a block, will paste them together with " || "
  # method = downsample$method # mean, subsamp
  # # dt = NULL

  #### checks
  assert_data_table(df) #for now, we are using data.table objects, so DT syntax applies
  assert_count(downsample_factor)


  t_cols <- c("time", "time_ev") #hard code single time column for now, dont see how this would need to be different. 4/2/21 include event-lock time start.
  d_cols <- digital_channels
  a_cols <- analog_channels
  c_cols <- char_channels

  ret_allevs <- data.table()
  for(ev in unique(df$eventn)){
    df_ev <- df %>% filter(eventn == ev) %>% mutate(time_ev = seq(0,n()-1, 1))


    #### Downsample time
    # tryCatch.ep({
    if (length(t_cols) > 0L) {
      time_data <- sapply(df_ev[, ..t_cols], function(col) { col[seq(1, length(col), downsample_factor)] }) %>% data.table()

      # time_data <- data.frame(time_data$time) %>% rename(`time` = `time_data.time`)  # sometimes dplyr is just easier...
    } else {
      time_data <- NULL
    }
    # }, describe_text = "-- 4.4.1 Downsample time column")

    #### Downsample analog data
  # tryCatch.ep({

    if (length(a_cols) > 0L) {
      # if(method == "mean"){
      #   setkeyv(df_ev, c("eventn", "time", "block_trial", "time_ev"))
      # }

      # 4/2/21 needs fix: test that all(digital_data$eventn == analog_data$eventn) is FALSE. I'm just not all that familiar with data.table syntax so MNH might need to weigh in.
      # a_cols_ev <- c("eventn", a_cols)
      # inp <- df_ev[,..a_cols_ev]
      # setkeyv(inp, "eventn")

      #key by eventn so there is no mixing of analogue data between (unrelated) events
      analog_data <- subsample_dt(df_ev[,..a_cols], dfac = downsample_factor, method = method)
      # analog_data <- subsample_dt(df[,..a_cols], dfac = downsample_factor, method = method)
    } else {
      message("unknown downsampling method: ", method)
      analog_data <- NULL
    }

  # }, describe_text = "-- 4.4.2 Downsample analogue channels")

  #### Downsample digital data
  # tryCatch.ep({
    if (length(d_cols) > 0L) {
      #could support subsampling here -- doesn't seem like a great idea, though
      digital_data <- data.table(do.call(cbind,lapply(df_ev[, ..d_cols], function(col) { downsample_digital_timeseries(col, downsample_factor, FALSE) })))
    } else {
      digital_data <- NULL
    }
  # }, describe_text = "-- 4.4.3 Downsample digital channels")

  #### Downsample/combine character data
  # tryCatch.ep({
    if(length(c_cols > 0L)){
      char_data <- downsample_chars(df_ev[,..c_cols], dfac = downsample_factor)
    }
  # }, describe_text = "-- 4.4.4 Downsample/combine character channels")


  ####combine all signals
  if (is.null(analog_data)) {
    ret <- digital_data
  } else if (is.null(digital_data)) {
    ret <- analog_data
  } else {
    stopifnot(nrow(analog_data) == nrow(digital_data)) # if this is violated, cbinding these will almost certainly be flawed.
    ret <- cbind(as.data.frame(digital_data), as.data.frame(analog_data))
    ret <- cbind(digital_data, analog_data)
  }

  if (!is.null(time_data)) {
    stopifnot(nrow(ret) == nrow(time_data))
    ret <- cbind(data.table(time_data), ret) }

  if (!is.null(char_data)) {
    stopifnot(nrow(ret) == nrow(char_data))
    ret <- cbind(ret, char_data) }

  ret_allevs <- rbind(ret_allevs, ret)
  }

  return(ret_allevs)
}


## adapted function originally written by MNH to downsample using subsampling (keep every n sample) or mean (average within-bin)
subsample_dt <- function(dt, keys=key(dt), dfac=1L, method="subsamp") {

  checkmate::assert_data_table(dt)
  if (method=="subsamp") {
    dt <- data.table(do.call(cbind,lapply(dt, function(col) {col[seq(1, length(col), dfac)] })))
  } else if (method=="mean") {
    downsamp <- function(col, dfac=1L) { col[seq(1, length(col), dfac)] }

    dt[, chunk := rep(1:ceiling(.N/dfac), each=dfac, length.out=.N), by=keys]
    dt <- dt[, lapply(.SD, mean), by=c(keys, "chunk")] #compute mean of every k samples
    dt[, chunk := NULL]
    # dt[, time := time - (dfac-1)/2]
  }
  return(dt)
}

downsample_chars <- function(dt, dfac=1L){
  # dt <- eye$raw[, ..c_cols]

  dt[, chunk := rep(1:ceiling(.N/dfac), each=dfac, length.out=.N)]#, by=keys]

  dt <- dt[, lapply(.SD, function(x) paste(unique(x), collapse = " | ")), by = chunk]
  dt[, et.msg := gsub(" | .", "", et.msg, fixed = TRUE)]
  dt[, et.msg := gsub(". | ", "", et.msg, fixed = TRUE)]
  dt[, chunk := NULL]
}


ep.eye_extend_blinks <- function(ep.eye,
                                 ms_before = 100,
                                 ms_after = 100){

  ### extract pupil size from raw.
  pup <- ep.eye$raw

  sr <- ep.eye$metadata$sample.rate
  bf <- ms_before
  af <- ms_after

  ### convert to ntimepoints depending on sampling rate if not 1000
  if(sr != 1000){
    conv_ms <- 1000/sr
    bf <- bf/conv_ms
    af <- af/conv_ms
  }


  blinks <- ep.eye$gaze$blink %>% tibble() %>% mutate(stime_ex = stime - bf,
                                                   etime_ex = etime + af)

  bl_rms <- c()
  for(row in 1:nrow(blinks)){ #tried to avoid, this is just so much easier
    rang <- blinks[row,] %>% select(stime_ex, etime_ex) %>% as.numeric()
    bl_rms <- c(bl_rms, seq(rang[1], rang[2], 1))
  }

  ep.eye$pupil$preprocessed <- pup %>% select(-xp, -yp) %>% tibble() %>% mutate(ps_blinkex = ifelse(time %in% bl_rms, NA, ps))

  return(ep.eye)
}


ep.eye_smooth_pupil <- function(ep.eye,
                                method = "movingavg",
                                window_length = 50){

  stopifnot(method == "movingavg")

  ### convert to ntimepoints depending on sampling rate if not 1000
  sr <- ep.eye$metadata$sample.rate
  maw <- window_length

  if(sr != 1000){
    conv_ms <- 1000/sr
    maw <- maw/conv_ms
  }

  ep.eye$pupil$preprocessed$ps_smooth <- movavg.ep(ep.eye$pupil$preprocessed$ps_blinkex, window_length, "s")

  ## average will run through the deblinked trials. Ensure these remain NA'ed
  ep.eye$pupil$preprocessed <-  ep.eye$pupil$preprocessed %>% mutate(ps_smooth = ifelse(is.na(ps_blinkex), NA, ps_smooth))


  return(ep.eye)
}

movavg.ep <- function (x, n, type = c("s", "t", "w", "m", "e", "r")){
  ### this is taken from the pracma package with added na.rm functionality

  stopifnot(is.numeric(x), is.numeric(n), is.character(type))
  if (length(n) != 1 || ceiling(n != floor(n)) || n <= 1)
    stop("Window length 'n' must be a single integer greater 1.")
  nx <- length(x)
  if (n >= nx)
    stop("Window length 'n' cannot be greater then length of time series.")
  y <- numeric(nx)
  if (type == "s.ep") {
    for (k in 1:(n - 1)) y[k] <- mean(x[1:k], na.rm = TRUE)
    for (k in n:nx) {
      if(all(x[k:(k+n)])){
        y[k] <- NA
      } else{
        y[k] <- mean(x[(k - n + 1):k], na.rm = TRUE)}
    }

  } else if (type == "s"){
    for (k in 1:(n - 1)) y[k] <- mean(x[1:k], na.rm = TRUE)
    for (k in n:nx) y[k] <- mean(x[(k - n + 1):k], na.rm = TRUE)
  }
  # else if (type == "t") {
  #   n <- ceiling((n + 1)/2)
  #   s <- movavg(x, n, "s")
  #   y <- movavg(s, n, "s")
  # }
  # else if (type == "w") {
  #   for (k in 1:(n - 1)) y[k] <- 2 * sum((k:1) * x[k:1],na.rm = TRUE)/(k *
  #                                                           (k + 1))
  #   for (k in n:nx) y[k] <- 2 * sum((n:1) * x[k:(k - n +
  #                                                  1)], na.rm= TRUE)/(n * (n + 1))
  # }
  # else if (type == "m") {
  #   y[1] <- x[1]
  #   for (k in 2:nx) y[k] <- y[k - 1] + (x[k] - y[k - 1])/n
  # }
  # else if (type == "e") {
  #   a <- 2/(n + 1)
  #   y[1] <- x[1]
  #   for (k in 2:nx) y[k] <- a * x[k] + (1 - a) * y[k - 1]
  # }
  # else if (type == "r") {
  #   a <- 1/n
  #   y[1] <- x[1]
  #   for (k in 2:nx) y[k] <- a * x[k] + (1 - a) * y[k - 1]
  # }
  # else stop("The type must be one of 's', 't', 'w', 'm', 'e', or 'r'.")
  return(y)
}



#' Interpolate over missing pupil data
#'
#' @param ep.eye ep.eye object containing ps_smooth as a column in pupil data.
#' @param algor preferred interpolation algorithm, see imputeTS::na_interpolation for options
#' @param maxgap Maximum amount of missing *time (in ms)* to interpolate over. Anything above this will be left NA. Converts internally from time to nmeasurements if data is not sampled at 1000Hz.
#'
#' @import imputeTS
#'
#'
#' @export

ep.eye_interp_pupil <- function(ep.eye,
                                algor = "linear",
                                maxgap =  1000){

  ### convert to ntimepoints depending on sampling rate if not 1000
  sr <- ep.eye$metadata$sample.rate
  # maxgap <- maxgap

  if(sr != 1000){
    conv_ms <- 1000/sr
    maxgap <- maxgap/conv_ms
  }

  all_t <- seq(0,max(ep.eye$raw$time))

  ##### N.B. if there are missing timepoints, they need to be included so we dont interpolate over periods where the tracker is off.
  paddf <- data.table(eventn = NA,
                      time = which(!all_t %in% ep.eye$raw$time) -1,
                      ps = NA,
                      saccn = NA,
                      fixn = NA,
                      blinkn = NA,
                      block = NA,
                      block_trial = NA,
                      event = NA,
                      et.msg = NA,
                      ps_blinkex = NA,
                      ps_smooth = NA
  )

  temp <- rbind(ep.eye$pupil$preprocessed, paddf) %>% arrange(time)

  temp$ps_interp <- imputeTS::na_interpolation(temp$ps_smooth, option = algor, maxgap = maxgap)

  ep.eye$pupil$preprocessed <- temp %>% filter(!is.na(eventn)) %>% data.table()

  return(ep.eye)
}


ep.eye_baseline_correct <- function(ep.eye,
                                    method = "subtract",
                                    dur_ms = 100,
                                    center_on){

  # if this is populated, the metadata will have information stored on the timestamp discrepancies and a warning that asks the user to check. Ideally, only one message is passed that marks the start of the trial/stimulus onset/etc
  mult_bldf <- data.table()
  # if this is populated, the trial in question does not have the specified center_on message and will use the first measurement alone as a baseline assessment
  missing_baseline <- c()

  ret_bc <- data.table()

  for(ev in unique(ep.eye$pupil$preprocessed$eventn)){

    st <- ep.eye$pupil$preprocessed %>% filter(eventn == ev) %>% tibble()

    baset <- st[which(grepl(center_on, st$et.msg)),]$time

    #### if there are more than one instance where message is passed, take first position, but log discrepancies and pass error at the end to look through these. This should be rare
    if(length(baset) > 1){
      mult_bldf <- rbind(mult_bldf, data.table(eventn = ev,
                                               time = baset,
                                               et.msg = st %>% dplyr::filter(time %in% baset) %>% pull(et.msg)))
      baset <- baset[1] # this could be considered for an argument to put in config file.
    }

    #### if no baseline message passed on this event, set baseline measurement to the first in the event
    if(length(baset) == 0){
      bl <- st$ps_interp[1]
      baset <- st$time[1]
      missing_baseline <- c(missing_baseline, ev)
    } else{
      bpos <- which(st$time %in% seq(baset - 100, baset))
      bl <- median(st$ps_interp[bpos], na.rm = TRUE)
    }

    ### perform baseline correction
    if(method == "subtract"){
      st <- st %>% mutate(ps_bc = ps_interp - bl,
                          time_bc = time -baset) %>% data.table()
    } else{
      message("Currently only subtrative ('subtract') baseline correction supported")
    }

    ret_bc <- rbind(ret_bc, st)
    # ggplot(st, aes(x = time_bc, y = ps_bc, color = eventn)) + geom_line() + geom_vline(xintercept = 0, alpha = .5) + geom_hline(yintercept = 0, alpha = .5) + theme_bw()
  }

  ep.eye$pupil$preprocessed <- ret_bc

  if(nrow(mult_bldf) != 0){
    ep.eye$metadata$pupil_multiple_baseline_msgs <- mult_bldf
    # cat("MESSAGE: For at least one trial, multiple baseline/center_on messages passed. First instance selected by default but see ep.eye$metadata$pupil_multiple_baselines for large discrepancies\n")
  }

  if(length(missing_baseline) != 0){
    ep.eye$metadata$pupil_missing_baseline_msg <- missing_baseline
    # cat("MESSAGE: For at least one trial, no baseline/center_on messages passed. First measurement in trial used as baseline.\n")
  }

  return(ep.eye)
}

