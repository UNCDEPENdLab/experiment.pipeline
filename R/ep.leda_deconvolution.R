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
sdeconv_analysis <- function(tau, leda, estim_tonic = 1){
  print("Inside sdeconv_analysis")
  # browser()
  # ---- Check Limits ----
  # tau <- leda$analysis0$tau
  x <- c(NA, NA)
  x[1] <- withinlimits(tau[1], leda$set$tauMin, 10)
  x[2] <- withinlimits(tau[2], leda$set$tauMin, 20)
  if (x[2] < x[1]){ # tau1 < tau2
    x <- rev(x)
  }
  if (abs(x[1] - x[2]) < leda$set$tauMinDiff){
    x[2] <- x[2] + leda$set$tauMinDiff
  }
  # browser()
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
  # browser()
  tb <- t - t[1] + dt

  bg <- bateman_gauss(tb, 5, 1, 2, 40, 0.4)
  idx <- which.max(bg)

  prefix <- (bg[1:(idx+1)] / bg[idx+1]) * d[1] #+10
  prefix[prefix < 10^-10] <- 0 # Added this line to match with MATLAB, seems like MATLAB has lower number of decimals allowed
  prefix <- prefix[prefix != 0]
  n_prefix <- length(prefix)
  d_ext <- c(prefix, d)

  t_ext <- seq((t[1] - dt), (t[1]-n_prefix*dt), by = -dt)
  t_ext <- c(fliplr(t_ext), t)
  tb <- t_ext - t_ext[1] + dt
  kernel <- bateman_gauss(tb, 0, 0, tau[1], tau[2], 0)

  # Adaptive kernel size
  midx <- which.max(kernel)
  kernelaftermx <- kernel[(midx+1):length(kernel)]
  kernel <- c(kernel[1:midx], kernelaftermx[kernelaftermx > 10^-5])
  kernel <- kernel / sum(kernel) # normalize to sum <- 1

  sigc <- max(c(0.1, leda$set$sigPeak/max(kernel)*10))  # threshold for burst peak

  # ---- Estimate tonic ----

  deconv_output <- pracma::deconv(c(d_ext, d_ext[length(d_ext)]*rep(1,length(kernel)-1)), kernel)
  driverSC <- deconv_output$q
  remainderSC <- deconv_output$r
  driverSC_smooth <- smooth(driverSC, swin, "gauss")

  # Shorten to data range
  driverSC <- driverSC[(n_prefix+1):length(driverSC)]
  print(tau)
  print(max(driverSC))
  # browser()
  driverSC_smooth <- driverSC_smooth[(n_prefix+1):length(driverSC_smooth)]
  remainderSC <- remainderSC[(n_prefix+1):(length(d)+n_prefix)]
  # Inter-impulse fit
  # browser()
  segment_driver_output <- segment_driver(driverSC_smooth,# Segmentation of non-extended data!
                                          rep(0, length(driverSC_smooth)),
                                          sigc, round(sr * leda$set$segmWidth))
  onset_idx <- segment_driver_output$segmOnset
  impulse <- segment_driver_output$segmImpulse
  overshoot <- segment_driver_output$segmOversh
  impMin <- segment_driver_output$impMin
  impMax <- segment_driver_output$impMax
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
  # print(paste("tau:", toString(tau)))
  leda$analysis0$driver <- phasicDriver # smoothed driver
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
  leda$analysis0$error$deviation <- c(err1d, 0)
  leda$analysis0$error$discreteness <- c(err_discreteness, 0)
  leda$analysis0$error$negativity <- err_negativity
  leda$analysis0$error$compound <- err
  print('completed sdeconv_analysis')
  return(list(err = err, x = x, leda = leda))

}


#' @title Deconvolution optimization
#'
#' @param x0
#' @param nr_iv
#' @param leda
#'
#' @author Nidhi Desai
#'
deconv_optimize <- function(x0, nr_iv, leda){ # the optimization steps seem to be working as expected. tau values converge on slightly different values but error tying to optimize is same as Matlab
  print("inside deconv optimize")
  # browser()
  if (nr_iv == 0){
    return(list(xopt = x0, opthistory = c()))
  }

  xList <- list(x0, c(1,2), c(1, 6), c(1, 8), c(0.5, 2), c(0.5, 4), c(0.5, 6), c(0.5, 8))

  x_opt <- list()
  err_opt <- c()
  opthistory <- list()
  leda_cgd_output <- list()
  # browser()
  for (i in 1:min(nr_iv, length(xList))){
    print(paste("loop", toString(i)))
    cgd_output <- cgd(xList[[i]], leda, sdeconv_analysis, c(0.3, 2), 0.01, 20, 0.05)
    # browser() # checked if cgd output is correct; cgd_output is almost same as MATLAB output, we will take it as a win
    opthistory[[i]] <- cgd_output$history
    x_opt[[i]] <- cgd_output$x
    leda_cgd_output[[i]] <- cgd_output$leda
    err_opt <- c(err_opt, cgd_output$history$error[length(cgd_output$history$error)])
  }
  # browser()
  # print("optimize loop completed")

  idx <- which.min(err_opt)
  xopt <- x_opt[[idx]]
  opthistory <- opthistory[[idx]]
  leda <- leda_cgd_output[[idx]]
  leda$analysis0$tau <- xopt
  leda$analysis0$opt_history <- opthistory
  print(paste("Final optimized parameter: ",
              sprintf(' %5.2f\t',xopt),
              sprintf(' Error: %6.3f',min(err_opt))))
  print("completed deconv optimize")
  return(list(xopt = xopt, opthistory = opthistory, leda = leda))
}


#' @title
#'
#' @param leda
#'
#' @author Nidhi Desai
#'
deconv_apply <- function(leda){
  print("inside deconv apply")
  # browser() # LAsT TIME CHECKING THIS
  # Prepare target data for full resolution analysis
  leda$analysis0$target$tonicDriver <- leda$analysis0$target$poly(leda$updated$time_ts) # ppval
  leda$analysis0$target$t <- leda$updated$time_ts
  leda$analysis0$target$d <- leda$updated$eda_ts
  leda$analysis0$target$sr <- leda$Hz
  sdeconv_output <- sdeconv_analysis(leda$analysis0$tau, leda, 0) # TODO check if this function call is correct
  leda <- sdeconv_output$leda

  leda$analysis0$target <- NULL
  leda$analysis0$driverSC <- NULL
  leda$analysis <- leda$analysis0
  leda$analysis$method <- 'sdeco'
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
  leda$analysis$impulsePeakTime <- t[maxL] # effective peak-latency
  leda$analysis$impulseAmp <- driver[maxL]

  # SCR data
  leda$analysis$onset <- leda$analysis$impulsePeakTime
  for (iPeak in 1:length(maxL)){
    driver_segment <- leda$analysis$driver[minL[iPeak]:minL[iPeak+1]]
    sc_reconv <- pracma::conv(driver_segment, leda$analysis$kernel)
    leda$analysis$amp[iPeak] <- max(sc_reconv)
    mx_idx <- which(sc_reconv == max(sc_reconv))
    leda$analysis$peakTime[iPeak] <- t[minL[iPeak]] + mx_idx[1]/leda$Hz # SCR peak could be outside of SC time range
  }
  negamp_idx <- which(leda$analysis$amp < 0.001) # criterion removes peaks at end of sc_reconv due to large negative driver-segments

  leda$analysis$impulseOnset <- leda$analysis$impulseOnset[-negamp_idx]
  leda$analysis$impulsePeakTime <- leda$analysis$impulsePeakTime[-negamp_idx]
  leda$analysis$impulseAmp <- leda$analysis$impulseAmp[-negamp_idx]
  leda$analysis$onset <- leda$analysis$onset[-negamp_idx]
  leda$analysis$amp <- leda$analysis$amp[-negamp_idx]
  leda$analysis$peakTime <- leda$analysis$peakTime[-negamp_idx]

  # add2log(1,'Continuous Decomposition Analysis.',1,1,1)
  # leda2.file.version = leda2.intern.version; %work around indicating analysis version of current fit
  print("completed deconv apply")
  return(leda)
}

