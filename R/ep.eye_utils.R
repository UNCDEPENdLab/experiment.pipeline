
# pupil funcs -------------------------------------------------------------


#### need to tweak for different sampling rates.
interp_pupil <- function(eye, maxgap, option = "linear"){
  all_t <- seq(0,max(eye$raw$time))

  ##### if there are missing timepoints, they need to be included so we dont interpolate over periods where the tracker is off.
  paddf <- data.table(eventn = NA,
                      time = which(!all_t %in% eye$raw$time) -1,
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

  temp <- rbind(eye$pupil$preprocessed, paddf) %>% arrange(time)

  temp$ps_interp <- na_interpolation(temp$ps_smooth, option = option, maxgap = maxgap)

  eye$pupil$preprocessed <- temp %>% filter(!is.na(eventn)) %>% data.table()

  return(eye)
}


baseline_correct <- function(eye, center_on, dur_ms){

  # if this is populated, the metadata will have information stored on the timestamp discrepancies and a warning that asks the user to check. Ideally, only one message is passed that marks the start of the trial/stimulus onset/etc
  mult_bldf <- data.table()
  # if this is populated, the trial in question does not have the specified center_on message and will use the first measurement alone as a baseline assessment
  missing_baseline <- c()

  ret_bc <- data.table()

  for(ev in unique(eye$pupil$preprocessed$eventn)){

    # st.msg <- eye$pupil$preprocessed %>% filter(eventn == ev & et.msg != ".") %>% tibble()
    st <- eye$pupil$preprocessed %>% filter(eventn == ev) %>% tibble()


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
    if(c.pupil$baseline_correction$method == "subtract"){
      st <- st %>% mutate(ps_bc = ps_interp - bl,
                          time_bc = time -baset) %>% data.table()
    } else{
      message("Currently only subtrative ('subtract') baseline correction supported")
    }

    ret_bc <- rbind(ret_bc, st)
    # ggplot(st, aes(x = time_bc, y = ps_bc, color = eventn)) + geom_line() + geom_vline(xintercept = 0, alpha = .5) + geom_hline(yintercept = 0, alpha = .5) + theme_bw()
  }

  eye$pupil$preprocessed <- ret_bc

  if(nrow(mult_bldf) != 0){
    eye$metadata$pupil_multiple_baseline_msgs <- mult_bldf
    cat("For at least one trial, multiple baseline/center_on messages passed. First instance selected by default but see eye$metadata$pupil_multiple_baselines for large discrepancies\n")
  }

  if(length(missing_baseline) != 0){
    eye$metadata$pupil_missing_baseline_msg <- missing_baseline
    cat("\nFor at least one trial, no baseline/center_on messages passed. First measurement in trial used as baseline.\n")
  }

  return(eye)
}



# provide ev-locked time --------------------------------------------------


tag_event_time <- function(eye){

  ################
  ######## RAW
  ################
  try({
    raw_estimes <- eye$raw  %>% group_by(eventn) %>%
      summarise(stime_ev = min(time),
                etime_ev = max(time), .groups = "drop")

    eye$raw <- raw_estimes %>% right_join(eye$raw, by = "eventn") %>%
      mutate(time_ev = (time - stime_ev)) %>%
      select(block, block_trial, event, eventn, time, time_ev, xp,yp,ps,saccn,fixn,blinkn,et.msg)
  })

  ################
  ######## DOWNSAMPLED: compute separately on downsampled data to preserve blocking structure.
  ################
  try({eye$gaze$downsample <- eye$gaze$downsample %>% group_by(eventn) %>%
    summarise(stime_ev = min(time),
              etime_ev = max(time), .groups = "drop") %>%
    right_join(eye$gaze$downsample, by = "eventn") %>% mutate(time_ev = (time - stime_ev)) %>%
    select(block, block_trial, event, eventn, time, time_ev, xp,yp, saccn, fixn,blinkn,et.msg)
  })

  try({eye$pupil$downsample <- eye$pupil$downsample %>% group_by(eventn) %>%
    summarise(stime_ev = min(time),
              etime_ev = max(time), .groups = "drop") %>%
    right_join(eye$pupil$downsample, by = "eventn") %>% mutate(time_ev = (time - stime_ev)) %>%
    select(block, block_trial, event, eventn, time, time_ev, time_bc, ps, ps_blinkex, ps_smooth, ps_interp, ps_bc, saccn, fixn,blinkn,et.msg)
  })


  ################
  ######## GAZE EVENTS: use raw_estimes
  ################

  try({

    ##saccades
    eye$gaze$sacc <- raw_estimes %>% right_join(eye$gaze$sacc, by = "eventn") %>%
      mutate(etime_ev = (etime - stime_ev),
             stime_ev = (stime - stime_ev)) %>%
      select(block, block_trial, event, eventn, saccn, stime, stime_ev,etime, etime_ev,  aoi_start, aoi_end, dur, sxp,syp, exp, eyp, ampl, pv)

    ##fixations
    eye$gaze$fix <- raw_estimes %>% right_join(eye$gaze$fix, by = "eventn") %>%
      mutate(etime_ev = (etime - stime_ev),
             stime_ev = (stime - stime_ev)) %>%
      select(block, block_trial, event, eventn, fixn, stime, stime_ev,etime, etime_ev,  aoi_look, dur, axp, ayp, aps)

    ##blinks
    eye$gaze$blink <- raw_estimes %>% right_join(eye$gaze$blink, by = "eventn") %>%
      mutate(etime_ev = (etime - stime_ev),
             stime_ev = (stime - stime_ev)) %>%
      select(block, block_trial, event, eventn, blinkn, stime, stime_ev,etime, etime_ev,dur)

  })

  ################
  ######## PREPROCESSED (NO DOWNSAMPLING) PUPIL: use raw_estimes
  ################

  try({
    eye$pupil$preprocessed <- raw_estimes %>% right_join(eye$pupil$preprocessed, by = "eventn") %>%
      mutate(time_ev = (time - stime_ev)) %>%
      select(block, block_trial, event, eventn, time, time_ev, time_bc, ps, ps_blinkex, ps_smooth, ps_interp, ps_bc, saccn, fixn, blinkn, et.msg)
  })

  return(eye)
}


#
#  interpolate_eye <- function(eye_dt, algor = "linear", maxgap = 1000){
#
#    c.interp <- config$definitions$eye[[signal]]$interpolate
#
#    if(!"algor" %in% c.interp){
#      c.interp$algor <- "linear"
#    }
#
#    ### Convert ms maxgap to ntimepoints
#    sr <- sample.rate
#    ds <- config$definitions$eye[[signal]]$downsample$factor
#    mg <- config$definitions$eye[[signal]]$interpolate$maxgap
#
#    if(sr != 1000){
#      conv_ms <- 1000/sr
#      ds <- ds*conv_ms
#      mg <- mg/conv_ms
#    }
#
#    ds_ntime <- ceiling(mg/(1000/ds))
#
#    if(df == "raw"){
#      eye[[signal]]$preprocessed <- na_interpolation(eye$raw, option = c.interp$algor, maxgap = mg)
#    } else {
#      eye[[signal]]$preprocessed <- na_interpolation(eye[[signal]][["preprocessed"]], option = c.interp$algor, maxgap = mg)
#    }
#
#
#    if(signal == "gaze"){
#      eye[[signal]]$preprocessed[,ps:=NULL]
#    } else if(signal == "pupil"){
#      eye[[signal]]$preprocessed[,xp:=NULL]
#      eye[[signal]]$preprocessed[,yp:=NULL]
#    }
#
#    return(eye)
#  }

