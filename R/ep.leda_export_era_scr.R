############################
##### List of subsidiary functions use to save Event related analysis and SCRs in ledalab
############################
# - export_scrlist()
# - export_era()
# - save_to_ep.physio()
############################

#' @title Export list of SCRs
#'
#' @param leda
#'
#' @author Nidhi Desai
#'
export_scrlist <- function(leda){

  scrAmplitudeMin <- leda$set$export$SCRmin_scr_output

  if (!is.null(leda$analysis)){
    onset <- leda$analysis$impulsePeakTime # impulse peak-time = peak-latency
    amp <- leda$analysis$amp
  }

  onset_ttp <- leda$trough2peakAnalysis$onset
  amp_ttp <- leda$trough2peakAnalysis$amp

  scrList <- list()
  if (!is.null(leda$analysis)){
    scr_idx <- which(onset >= 0 & amp >= scrAmplitudeMin)
    if (is.null(scr_idx)){
      print("No SCRs detected")
    } else {
      scrList$CDA$onset <- onset[scr_idx]
      scrList$CDA$amp <- amp[scr_idx]
    }
  }

  scr_ttpidx <- which(onset_ttp >= 0 & amp_ttp >= scrAmplitudeMin)
  if (is.null(scr_ttpidx)){
    print("No SCRs detected (method TTP)!")
  }
  scrList$TTP$onset <- onset_ttp[scr_ttpidx]
  scrList$TTP$amp <- amp_ttp[scr_ttpidx]

  # z-scaling
  if (leda$set$export$zscale){
    # better than zscore from the stats toolbox as it's free and handles NaN better
    zscore = function(x) (x-mean(x[!is.na(x)]))/sd(x[!is.na(x)])
    scrList$CDA$amp = zscore(scrList$CDA$amp)
    scrList$TTP$amp = zscore(scrList$TTP$amp)
  }

  leda$scrList <- scrList
  return(leda)
}


#' @title Export event related analysis data
#'
#' @param leda
#'
#' @author Nidhi Desai
#'
export_era <- function(leda){

  if (leda$events_data$N == 0){
    print("No events data!")
  }
  scrWindow_t1 <- leda$set$export$SCRstart
  scrWindow_t2 <- leda$set$export$SCRend
  scrAmplitudeMin <- leda$set$export$SCRmin_era_output
  sr <- leda$Hz

  onset_ttp <- leda$trough2peakAnalysis$onset
  amp_ttp <- leda$trough2peakAnalysis$amp

  if (!is.null(leda$analysis)){
    onset_sdeco <- leda$analysis$impulsePeakTime # impulse peak-time = peak-latency
    amp_sdeco <- leda$analysis$amp
  }
  era <- list()
  era$Event <- list()
  era$Event$time <- rep(NA, leda$events_data$N)
  era$Event$nid <- rep(NA, leda$events_data$N)
  era$Event$name <- rep(NA, leda$events_data$N)
  era$Event$ud <- rep(NA, leda$events_data$N)
  for (iEvent in 1:leda$events_data$N){
    # Set event data
    event <- leda$events_data$event[iEvent,]
    era$Event$time[iEvent] <- event$time
    era$Event$nid[iEvent] <- event$nid
    era$Event$name[iEvent] <- event$name
    if (!is.null(event$userdata)){ era$Event$ud[iEvent] <- event$userdata }

    subrange_output <- subrange(event$time + scrWindow_t1, event$time + scrWindow_t2,
                                          leda$updated$time_ts, leda$updated$eda_ts)  # data of response window
    cs_respwin <- subrange_output$cs
    idx_respwin <- subrange_output$t_idx

    # RESET ALL MEASURES
    # Measures yielded by Continuous Decomposition Analysis (CDA)
    if (!is.null(leda$analysis)){
      era$CDA$nSCR[iEvent] <- NA        # Number of significant (= above threshold) SCRs within response window
      era$CDA$Latency[iEvent] <- NA     # Latency of first sign SCR within response window (= time of corresponding impulse-peak)
      era$CDA$AmpSum[iEvent] <- NA      # Amplitude-Sum of sign SCRs (reconvolved from phasic driver-peaks)
      era$CDA$SCR[iEvent] <- NA         # Average phasic driver activity (time integral over response window by size of responsewindow)
      era$CDA$ISCR[iEvent] <- NA        # Phasic driver area (time integral over response window)
      era$CDA$PhasicMax[iEvent] <- NA   # Driver maximum within response window
      era$CDA$Tonic[iEvent] <- NA       # Average level of (decomposed) Tonic component
    }

    # Measures yielded by Trough-To-Peak Analysis
    era$TTP$nSCR[iEvent] <- NA          # Number of significant (= above threshold) SCRs within response window
    era$TTP$Latency[iEvent] <- NA       # Latency of first sign SCR within response window
    era$TTP$AmpSum[iEvent] <- NA        # Amplitude-Sum of sign SCRs (EDA-Max - EDA-Min)

    # Measures based on raw SC data
    era$Global$Mean[iEvent] <- NA
    era$Global$MaxDeflection[iEvent] <- NA

    if (is.null(idx_respwin)){
      warning(paste("Data doesn''t contain ERA-window. Is the marker too close to the end of the recording?\n Event:", toString(iEvent)))
      break
    }

    # Set Measures
    # TTP
    scr_idx <- which(onset_ttp >= (event$time + scrWindow_t1) &
                       onset_ttp <= (event$time + scrWindow_t2) &
                       amp_ttp >= scrAmplitudeMin)
    nPeaks <- length(scr_idx)

    era$TTP$nSCR[iEvent] <- nPeaks
    era$TTP$AmpSum[iEvent] <- sum(amp_ttp[scr_idx])
    if (nPeaks > 0){
      era$TTP$Latency[iEvent] <- onset_ttp[scr_idx[1]] - event$time
    }

    # Global measures
    era$Global$Mean[iEvent] <- mean(leda$updated$eda_ts[idx_respwin]) # simple Mean of data within response window
    diff <- rep(0, length(cs_respwin)-1)
    for (i in 1:(length(cs_respwin)-1)){
      diff[i] <- max(cs_respwin[(i+1):length(cs_respwin)]) - cs_respwin[i]
    }
    era$Global$MaxDeflection[iEvent] <- max(c(diff, 0))

    # Decomposition measures
    if (!is.null(leda$analysis)){

      scr_idx <- which(onset_sdeco >= (event$time + scrWindow_t1) &
                         onset_sdeco <= (event$time + scrWindow_t2) &
                         amp_sdeco >= scrAmplitudeMin)
      nPeaks <- length(scr_idx)
      era$CDA$nSCR[iEvent] <- nPeaks
      if (nPeaks > 0){
        era$CDA$Latency[iEvent] <- onset_sdeco[scr_idx[1]] - event$time
      }
      era$CDA$AmpSum[iEvent] <- sum(amp_sdeco[scr_idx])

      era$CDA$ISCR[iEvent] <- max(c(0, sum(leda$analysis$driver[idx_respwin])/sr)) # ISCR = phasic_area  [muS*sec]
      era$CDA$SCR[iEvent] <- era$CDA$ISCR[iEvent] / (scrWindow_t2-scrWindow_t1) # SCR = average phasic driver activity  [muS]
      era$CDA$PhasicMax[iEvent] <- max(c(0, max(leda$analysis$driver[idx_respwin])))
      era$CDA$Tonic[iEvent] <- mean(leda$analysis$tonicData[idx_respwin])
    }
  }

  # z-scaling
  if (leda$set$export$zscale){
    # better than zscore from the stats toolbox as it's free and handles NaNs better
    zscore <- function(x) (x-mean(x[!is.na(x)]))/sd(x[!is.na(x)])
    era$CDA = structfun(zscore, era$CDA, 'UniformOutput', false)
    # TODO DONT KNOW HOW TO TRANSLATE THE ABOVE LINE OF CODE
    era$TTP$AmpSum <- zscore(era$TTP$AmpSum)
  }

  leda$analysis$era <- era
  return(leda)
}

#' Save the output SCRs and ERA information to ep.physio data structure
#' @param ep.physio ep.physio data structure
#' @param leda leda data structure
#'
#' @author Nidhi Desai
#'
save_to_ep.physio <- function(ep.physio, leda){

  ep.physio$eda$event_scr$Event <- leda$analysis$era$Event
  ep.physio$eda$event_scr$CDA <- leda$analysis$era$CDA
  ep.physio$eda$event_scr$TTP <- leda$analysis$era$TTP
  ep.physio$eda$event_scr$Global <- leda$analysis$era$Global

  ep.physio$eda$scr <- leda$scrList
  ep.physio$eda$scl <- leda$analysis$tonicData

  ep.physio$eda$phasic_driver <- leda$analysis$driver # phasic driver


  return(ep.physio)
}






