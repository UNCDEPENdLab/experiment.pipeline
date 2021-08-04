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