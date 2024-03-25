############################
##### List of subsidiary functions utilized in `ep.eye_cleanup()`
############################
# - ep.eye_tag_event_time()
# - ep.eye_save_preproc()
############################

#' Tag Event Time for Eye Data
#'
#' This function tags the event time for each eye-tracking event in the provided data.
#' The function works on raw data, downsampled gaze and pupil data, and preprocessed pupil data.
#' It also separately processes gaze event data including saccades, fixations, and blinks.
#'
#' @param ep.eye An object containing the eye tracking data to be processed.
#'
#' @return Returns the eye tracking data with event times tagged.
#'
#' @examples
#' \dontrun{
#' tagged_data <- ep.eye_tag_event_time(raw_eye_data)
#' }
#' @export
#'
#' @seealso \code{\link{right_join}}, \code{\link{mutate}}, \code{\link{select}}, \code{\link{group_by}}, \code{\link{summarise}}
ep.eye_tag_event_time <- function(ep.eye){

  ################
  ######## RAW
  ################
  try({
    raw_estimes <- ep.eye$raw  %>% group_by(eventn) %>%
      summarise(stime_ev = min(time),
                etime_ev = max(time), .groups = "drop")

    ep.eye$raw <- raw_estimes %>% right_join(ep.eye$raw, by = "eventn") %>%
      mutate(time_ev = (time - stime_ev)) %>%
      select(block, block_trial, event, eventn, time, time_ev, xp,yp,ps,saccn,fixn,blinkn,et.msg)
  })

  ################
  ######## DOWNSAMPLED: compute separately on downsampled data to preserve blocking structure.
  ################
  try({ep.eye$gaze$downsample <- ep.eye$gaze$downsample %>% group_by(eventn) %>%
    summarise(stime_ev = min(time),
              etime_ev = max(time), .groups = "drop") %>%
    right_join(ep.eye$gaze$downsample, by = "eventn") %>% mutate(time_ev = (time - stime_ev)) %>%
    select(block, block_trial, event, eventn, time, time_ev, xp,yp, saccn, fixn,blinkn,et.msg)
  })

  try({ep.eye$pupil$downsample <- ep.eye$pupil$downsample %>% group_by(eventn) %>%
    summarise(stime_ev = min(time),
              etime_ev = max(time), .groups = "drop") %>%
    right_join(ep.eye$pupil$downsample, by = "eventn") %>% mutate(time_ev = (time - stime_ev)) %>%
    select(block, block_trial, event, eventn, time, time_ev, time_bc, ps, ps_blinkex, ps_smooth, ps_interp, ps_bc, saccn, fixn,blinkn,et.msg)
  })


  ################
  ######## GAZE EVENTS: use raw_estimes
  ################

  try({

    ##saccades
    ep.eye$gaze$sacc <- raw_estimes %>% right_join(ep.eye$gaze$sacc, by = "eventn") %>%
      mutate(etime_ev = (etime - stime_ev),
             stime_ev = (stime - stime_ev)) %>%
      select(block, block_trial, event, eventn, saccn, stime, stime_ev,etime, etime_ev,  aoi_start, aoi_end, dur, sxp,syp, exp, eyp, ampl, pv)

    ##fixations
    ep.eye$gaze$fix <- raw_estimes %>% right_join(ep.eye$gaze$fix, by = "eventn") %>%
      mutate(etime_ev = (etime - stime_ev),
             stime_ev = (stime - stime_ev)) %>%
      select(block, block_trial, event, eventn, fixn, stime, stime_ev,etime, etime_ev,  aoi_look, dur, axp, ayp, aps)

    ##blinks
    ep.eye$gaze$blink <- raw_estimes %>% right_join(ep.eye$gaze$blink, by = "eventn") %>%
      mutate(etime_ev = (etime - stime_ev),
             stime_ev = (stime - stime_ev)) %>%
      select(block, block_trial, event, eventn, blinkn, stime, stime_ev,etime, etime_ev,dur)

  })

  ################
  ######## PREPROCESSED (NO DOWNSAMPLING) PUPIL: use raw_estimes
  ################

  try({
    ep.eye$pupil$preprocessed <- raw_estimes %>% right_join(ep.eye$pupil$preprocessed, by = "eventn") %>%
      mutate(time_ev = (time - stime_ev)) %>%
      select(block, block_trial, event, eventn, time, time_ev, time_bc, ps, ps_blinkex, ps_smooth, ps_interp, ps_bc, saccn, fixn, blinkn, et.msg)
  })

  return(ep.eye)
}


ep.eye_save_preproc <- function(ep.eye,
                                prefix,
                                out_dir) {
    if(is.null(out_dir)) {
      spath <- paste0(prefix, "_ep.eye.preproc.RData")
    } else{
      if(!dir.exists(out_dir)) dir.create(out_dir)
      spath <- file.path(out_dir, paste0(prefix, "_ep.eye.preproc.RData"))
    }
    save(ep.eye, file = spath);
    return(spath)
}
