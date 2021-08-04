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
############################

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

ep.eye_gen_aoi_look <- function(ep.eye, aoi_ref, tag_raw = FALSE, dt = NULL){
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

#' collapse timing to one row per time (paste multiple messages in a single et.msg)
#'
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


ep.eye_downsample <- function(df, 
                              downsample_factor=50,
                              digital_channels = c("eventn", "saccn", "fixn", "blinkn", "block_trial"), # integer values of trial, event, and gevs.
                              analog_channels = c("xp", "yp", "ps"), # gaze and pupil measurements
                              char_channels = c("et.msg", "block", "event"), # characters, if there are unique values within a block, will paste them together with " || "
                              # min_samps = 10, # FEATURE WISH-LIST: could be useful to implement so chunks with large amt of  missing measurements get dropped/set to NA.
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
                                 sample.rate = 1000,
                                 ms_before = 100, 
                                 ms_after = 100){

  ### extract pupil size from raw.
  pup <- ep.eye$raw

  sr <- sample.rate
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
