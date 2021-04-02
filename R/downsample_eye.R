# downsampling functions --------------------------------------------------

downsample_eye <- function(df, downsample_factor=50,
                           digital_channels = c("eventn", "saccn", "fixn", "blinkn", "block_trial"), # integer values of trial, event, and gevs.
                           analog_channels = c("xp", "yp", "ps"), # gaze and pupil measurements
                           char_channels = c("et.msg", "block", "event"), # characters, if there are unique values within a block, will paste them together with " || "
                           # min_samps = 10, # FEATURE WISH-LIST: could be useful to implement so chunks with large amt of  missing measurements get dropped/set to NA.
                           method = "mean" # mean, subsamp
){

  # browser()

  # stopifnot(inherits(eye, "ep.eye"))
  # # if (is.null(acq_data$ttl_codes)) { stop("Cannot find $ttl_codes element in ep.physio object. Run augment_ttl_details?") }
  # if (is.null(eye$raw)) { stop("Cannot find $raw element in ep.eye object") }

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
#
# char_data <- downsample_chars(df[,..c_cols], dfac = downsample_factor)
# ret_char <- cbind(ret_nochar, char_data)
#
# ret_char %>% tibble() %>% filter(grepl(" | ", event))
# ret %>% tibble() %>% filter(eventn == 7)

