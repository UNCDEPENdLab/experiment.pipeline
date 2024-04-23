############################
##### List of subsidiary functions use for performing CDA analysis in ledalab
############################
# - sdeconv_analysis()
# - deconv_optimize()
# - deconv_apply()
############################

#' @title 
#' 
#' @param leda
#' @param estim_tonic
#' 
#' @author Nidhi Desai
#' 
sdeconv_analysis <- function(leda, estim_tonic = 1){
  
  # ---- Check Limits ----
  tau <- leda$analysis0$tau
  x[1] <- withinlimits(tau[1], leda$set$tauMin, 10)
  x[2] <- withinlimits(tau[2], leda$set$tauMin, 20)
  if (x[2] < x[1]){ # tau1 < tau2
    x <- rev(x)
  }
  if (abs(x[1] - x[2]) < leda$set$tauMinDiff){
    x[2] <- x[2] + leda$set$tauMinDiff
  }
  tau[1] <- x[1]
  tau[2] <- x[2]

  data <- leda$analysis0$target$d # leda.data.conductance.data
  t <- leda$analysis0$target$t  # leda.data.time.data
  sr <- leda$analysis0$target$sr # leda.data.samplingrate
  smoothwin <- leda$analysis0$smoothwin * 8 # Gauss 8 SD
  dt <- 1/sr
  winwidth_max <- 3 #sec
  swin <- round(min(smoothwin, winwidth_max) * sr)
  
  d <- data
  
  # ---- Data preparation ----
  tb <- t - t[1] + dt
  bg <- bateman_gauss(tb, 5, 1, 2, 40, 0.4)
  idx <- which.max(bg)
  
  prefix <- (bg[1:(idx+1)] / bg[idx+1]) * d[1] #+10
  prefix <- prefix[prefix != 0] 
  n_prefix <- length(prefix)
  d_ext <- c(prefix, d)

  t_ext <- seq((t[1] - dt), (t[1]-n_prefix*dt), by <- -dt)
  t_ext <- c(fliplr(t_ext), t)
  tb <- t_ext - t_ext[1] + dt
  
  kernel <- bateman_gauss(tb, 0, 0, tau[1], tau[2], 0)
  # Adaptive kernel size
  midx <- which.max(kernel)
  kernelaftermx <- kernel[midx+1:length(kernel)]
  kernel <- c(kernel[1:midx], kernelaftermx[kernelaftermx > 10^-5])
  kernel <- kernel / sum(kernel) # normalize to sum <- 1
  
  sigc <- max(c(0.1, leda$set$sigPeak/max(kernel)*10))  # threshold for burst peak
  
  # ---- Estimate tonic ----
  [driverSC, remainderSC] <- pracma::deconv([d_ext, d_ext(end)*ones(1,length(kernel)-1)], kernel)
  driverSC_smooth <- smooth(driverSC, swin, "gauss")
  # Shorten to data range
  driverSC <- driverSC[n_prefix+1:length(driverSC)]
  driverSC_smooth <- driverSC_smooth[n_prefix+1:lrngth(driverSC_smooth)]
  remainderSC <- remainderSC[(n_prefix+1):(length(d)+n_prefix)]
  # Inter-impulse fit
  [onset_idx, impulse, overshoot, impMin, impMax] <- segment_driver(driverSC_smooth,# Segmentation of non-extended data!
                                                                    rep(0, length(driverSC_smooth)),
                                                                    sigc, round(sr * leda$set$segmWidth), digits <- 2)  
  if (estim_tonic == 1){ # Estimating tonic data
    sdeco_fit <- sdeco_interimpulsefit(leda, driverSC_smooth, kernel, impMin, impMax)
    leda <- sdeco_fit$leda
    tonicDriver <- sdeco_fit$tonicDriver
    tonicData <- sdeco_fit$tonicData
  } else {
    tonicDriver <- leda$analysis0$target$tonicDriver # TODO where did this tonicDriver come from?
    nKernel <- length(kernel)
    tonicData <- pracma::conv(c(tonicDriver[1]*rep(1,nKernel), tonicDriver), kernel)
    tonicData <- tonicData[nKernel:(length(tonicData)-nKernel)]
  }

  # Build tonic and phasic data
  phasicData <- d - tonicData
  phasicDriverRaw <- driverSC - tonicDriver
  phasicDriver <- smooth(phasicDriverRaw, swin, "gauss")

  # ---- Compute model error ----
  err_MSE <- fiterror(data, tonicData+phasicData, 0, "MSE")
  err_RMSE <- sqrt(err_MSE)
  err_chi2 <- err_RMSE / leda$data$conductance$error # TODO Where is this conductance error coming from
  err1d <- deverror(phasicDriver, 0.2)
  err1s <- succnz(phasicDriver, max(0.01, max(phasicDriver)/20), 2, sr)
  phasicDriverNeg <- phasicDriver
  phasicDriverNeg[phasicDriverNeg > 0] <- 0
  err_discreteness <- err1s
  err_negativity <- sqrt(mean(phasicDriverNeg^2))
  
  # ---- criterion ----
  alpha <- 5
  err <- err_discreteness + err_negativity * alpha
  
  # ---- save variables ----
  leda$analysis0$tau <- tau
  leda$analysis0$driver <- phasicDriver # i$e$ smoothed driver
  leda$analysis0$tonicDriver <- tonicDriver
  leda$analysis0$driverSC <- driverSC_smooth
  leda$analysis0$remainder <- remainderSC
  leda$analysis0$kernel <- kernel
  leda$analysis0$phasicData <- phasicData
  leda$analysis0$tonicData <- tonicData
  leda$analysis0$phasicDriverRaw <- phasicDriverRaw
  
  # ---- error ----
  leda$analysis0$error <- list()
  leda$analysis0$error$MSE <- err_MSE
  leda$analysis0$error$RMSE <- err_RMSE
  leda$analysis0$error$chi2 <- err_chi2
  leda$analysis0$error$deviation <- [err1d, 0]
  leda$analysis0$error$discreteness <- [err_discreteness, 0]
  leda$analysis0$error$negativity <- err_negativity
  leda$analysis0$error$compound <- err

  return(list(x = x, leda = leda))
  
}

#' @title Deconvolution optimization
#' 
#' @param x0
#' @param nr_iv
#' @param method
#' 
#' @author Nidhi Desai
#' 
deconv_optimize <- function(x0, nr_iv, method = 'sdeco'){
  
  if (nr_iv == 0){
    return(list(xopt = x0, opthistory = c()))
  }
  
  xList <- list(x0, c(1,2), c(1, 6), c(1, 8), c(0.5, 2), c(0.5, 4), c(0.5, 6), c(0.5, 8))
  
  x_opt <- list()
  err_opt <- c()
  
  opthistory <- list()
  for (i in 1:min(nr_iv, length(xList))){
    cgd_output <- cgd(xList[[i]], sdeconv_analysis, c(0.3, 2), 0.01, 20, 0.05)
    opthistory[[i]] <- cgd_output$history
    x_opt[i] <- cgd_output$x
    err_opt <- c(err_opt, history$error[length(history$error))])
  }
  
  idx <- which.min(err_opt)
  xopt <- x_opt[[idx]]
  # add2log(0, ['Final optimized parameter: ',sprintf(' %5.2f\t',xopt),sprintf(' Error: %6.3f',mn)], 0,1,1,1,0)
  
  return(list(xopt = xopt, opthistory = opthistory))
}


# After this go back to leda.sdeco.R and complete calling this function
# Next steps would be to test everything under leda.sdeco (downsampling, deconvolution, bateman functions) - DO ON FRIDAY

#' @title
#' 
#' @param leda 
#' 
#' @author Nidhi Desai
#' 
deconv_apply <- function(leda){
  
  # Prepare target data for full resolution analysis
  leda$analysis0$target$tonicDriver = ppval(leda$analysis0$target$poly, leda$updated$time_ts) # TODO Where did this target$poly come from
  leda$analysis0$target$t = leda$updated$time_ts
  leda$analysis0$target$d = leda$updated$eda_ts
  leda$analysis0$target$sr = leda$Hz
  
  sdeconv_analysis(leda$analysis0$tau, 0) # TODO check if this function call is correct
  
  leda$analysis0$target <- NULL
  leda$analysis0$driverSC <- NULL
  leda$analysis <- leda$analysis0
  leda$analysis$method = 'sdeco'
  leda$analysis0 <- NULL
  
  # SCRs reconvolved from Driver-Peaks
  t <- leda$updated$time_ts
  driver <- leda$analysis$driver
  get_peaks_output <- get_peaks(driver)
  minL <- get_peaks_output$min
  maxL <- get_peaks_output$max
  minL <- c(minL[1:length(maxL)], length(t))
  
  # Impulse data
  leda$analysis$impulseOnset <- t[minL[1:(length(minL)-1)]]
  leda$analysis$impulsePeakTime <- t[maxL]   # effective peak-latency
  leda$analysis$impulseAmp <- driver[maxL]
  
  # SCR data
  leda$analysis$onset <- leda$analysis$impulsePeakTime
  for (iPeak in 1:length(maxL)){
    driver_segment <- leda$analysis$driver[minL[iPeak]:minL[iPeak+1]]
    sc_reconv <- conv(driver_segment, leda$analysis$kernel)
    leda$analysis$amp[iPeak] <- max(sc_reconv)
    mx_idx <- which(sc_reconv == max(sc_reconv))
    leda$analysis$peakTime[iPeak] <- t[minL[iPeak]] + mx_idx[1]/leda$Hz # SCR peak could be outside of SC time range
  }
  negamp_idx <- which(leda$analysis$amp < 0.001) # criterion removes peaks at end of sc_reconv due to large negative driver-segments
  leda$analysis$impulseOnset[negamp_idx] <- NULL
  leda$analysis$impulsePeakTime[negamp_idx] <- NULL
  leda$analysis$impulseAmp[negamp_idx] <- NULL
  leda$analysis$onset[negamp_idx] <- NULL
  leda$analysis$amp[negamp_idx] <- NULL
  leda$analysis$peakTime[negamp_idx] <- NULL
  
  # add2log(1,'Continuous Decomposition Analysis.',1,1,1)
  # leda2.file.version = leda2.intern.version; %work around indicating analysis version of current fit

  return(leda)
}





