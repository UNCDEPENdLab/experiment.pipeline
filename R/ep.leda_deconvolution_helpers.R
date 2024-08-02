############################
##### List of subsidiary functions use for performing CDA analysis in ledalab
############################
# - segment_driver()
# - sdeco_interimpulsefit()
# - fiterror()
# - deverror()
# - succnz()
# - cgd()
# -- cgd_get_gradient()
# -- cgd_linesearch()
############################

#' @title
#'
#' @param data
#' @param remd
#' @param sigc
#' @param segWidth
#' @return
#'
#' @author Nidhi Desai
#'
segment_driver <- function(data, remd, sigc, segmWidth){

  segmOnset <- c()
  segmImpulse <- list()
  segmOversh <- list()
  impMin <- c()
  impMax <- c()

  get_peaks_output <- get_peaks(data)
  cccrimin <- get_peaks_output$min
  cccrimax <- get_peaks_output$max
  if (length(cccrimax) == 0){
    output <- list(segmOnset = segmOnset,
                   segmImpulse = segmImpulse,
                   segmOversh = segmOversh,
                   impMin = impMin,
                   impMax = impMax)
    return(output)
  }

  signpeak <- function(data, cccrimin, cccrimax, sigc){
    minL <- matrix(ncol = 2, nrow = 0)
    maxL <- c()

    if (length(cccrimax) == 0){
      output <- list(minL = minL, maxL = maxL)
      return(output)
    }

    dmm <- matrix(c(data[cccrimax] - data[cccrimin[1:length(cccrimin)-1]],
                  data[cccrimax] - data[cccrimin[2:length(cccrimin)]]), ncol = 2)
    maxL <- cccrimax[apply(dmm, 1, max) > sigc]

    # keep only minima right before and after sign maxima
    for (i in c(1:length(maxL))){
      minm1_idx <- which(cccrimin < maxL[i])
      before_smpl <- cccrimin[minm1_idx[length(minm1_idx)]]
      after_smpl <- cccrimin[minm1_idx[length(minm1_idx)]+1]
      minL <- rbind(minL, c(before_smpl, after_smpl)) # this adds new rows to the matrix
    }

    output <- list(minL = minL, maxL = maxL)
    return(output)
  }

  output2 <- signpeak(data, cccrimin, cccrimax, sigc)
  minL <- output2$minL
  maxL <- output2$maxL

  get_peaks_output <- get_peaks(remd)
  rmdimin <- get_peaks_output$min
  rmdimax <- get_peaks_output$max
  output2 <- signpeak(remd, rmdimin, rmdimax, 0.005) # get remainder segments
  rmdimins <- output2$minL
  rmdimaxs <- output2$maxL

  # Segments: 12 sec, max 3 sec preceding maximum
  for (i in 1:length(maxL)){
    segm_start <- max(c(minL[i,1], maxL[i] - round(segmWidth/2)))
    segm_end <- min(c(segm_start + segmWidth - 1, length(data)))

    # impulse
    segm_idx <- c(segm_start:segm_end)
    segm_data <- data[segm_idx]
    segm_data[segm_idx >= minL[i,2]] <- 0
    segmOnset[i] <- segm_start
    segmImpulse[[i]] <- segm_data

    # overshoot
    oversh_data <- rep(0, length(segm_idx))
    if (i < length(maxL)){
      rmi <- rmdimaxs[rmdimaxs > maxL[i] & rmdimaxs < maxL[i+1]]
    } else {
      rmi <- rmdimaxs[rmdimaxs > maxL[i]]
    }
    # no zero overshoots
    if (length(rmi) == 0){
      if (i < length(maxL)){
        rmi <- rmdimax[rmdimax > maxL[i] & rmdimax < maxL[i+1]]
      } else {
        rmi <- rmdimax[rmdimax > maxL[i]]
      }
      rmdimaxs <- rmdimax
      rmdimins <- c(rmdimin[1:length(rmdimin)-1], rmdimin[2:length(rmdimin)])
    }

    if (length(rmi) != 0){
      rmi <- rmi[1]
      oversh_start <- max(c(rmdimins[rmi,1], segm_start))
      oversh_end <- min(c(rmdimins[rmi,2], segm_end)) # min(rmdimins(rmi+1), segm_end)
      oversh_data[(oversh_start-segm_start+1) : (length(oversh_data)-segm_end-oversh_end)] <- remd[oversh_start:oversh_end]
    }

    segmOversh[[i]] <- oversh_data
  }

  impMin <- minL
  impMax <- maxL
  # print("complete segment driver")
  output <- list(segmOnset = segmOnset,
                 segmImpulse = segmImpulse,
                 segmOversh = segmOversh,
                 impMin = impMin,
                 impMax = impMax)
  return(output)
}


#' @title
#'
#' @param leda
#' @param driver
#' @param kernel
#' @param minL
#' @param maxL
#' @return
#'
#' @importFrom pracma pchip conv
#'
#' @author Nidhi Desai

sdeco_interimpulsefit <- function(leda, driver, kernel, minL, maxL){
  # print("inside sdeco_interimpulsefit")
  t <- leda$analysis0$target$t
  d <- leda$analysis0$target$d
  sr <- leda$analysis0$target$sr
  tonicGridSize <- 10 #leda$set$tonicGridSize_sdeco
  nKernel <- length(kernel)

  # Get inter-impulse data index
  iif_idx <- c()
  if (length(maxL) > 2){
    for (i in c(1:(length(maxL)-1))){
      gap_idx <- c(minL[i,2]:minL[i+1,1]) # +1: removed otherwise no inter-impulse points may be available at highly smoothed data
      iif_idx <- c(iif_idx, gap_idx)
    }
    iif_idx <- c(minL[2,1], iif_idx)
    if (minL[nrow(minL),2] < (length(driver)-sr)){
      iif_idx <- c(iif_idx, c(minL[nrow(minL),2]:(length(driver)-sr)))
    }
  } else { # no peaks (exept for pre-peak and may last peak) so data represents tonic only, so ise all data for tonic estimation
    iif_idx <- t[t > 0]
  }

  iif_t <- t[iif_idx]
  iif_data <- driver[iif_idx]

  groundtime <- c(seq(0, t[length(t)-1], by = tonicGridSize), t[length(t)])

  if (tonicGridSize < 30){
    tonicGridSize <- tonicGridSize * 2
  }

  groundlevel <- rep(0, length(groundtime))
  for (i in c(1:length(groundtime))){
    # Select relevant interimpulse time points for tonic estimate at groundtime
    if (i == 1){
      t_idx <- (iif_t <= groundtime[i]+tonicGridSize) & (iif_t > 1)
      grid_idx <- (t <= groundtime[i]+tonicGridSize) & (t > 1)
    } else if (i == length(groundtime)){
      t_idx <- (iif_t > groundtime[i]-tonicGridSize) & (iif_t < t[length(t)]-1)
      grid_idx <- (t > groundtime[i]-tonicGridSize) & (t < t[length(t)]-1)
    } else {
      t_idx <- (iif_t > groundtime[i]-tonicGridSize/2) & (iif_t <= groundtime[i]+tonicGridSize/2)
      grid_idx <- (t > groundtime[i]-tonicGridSize/2) & (t <= groundtime[i]+tonicGridSize/2)
    }

    # Estimate groundlevel at groundtime
    if (length(find(t_idx)) > 2){
      groundlevel[i] <- min(c(mean(iif_data[t_idx]), d[time_idx(t, groundtime[i])$idx]))
    } else { # if no inter-impulses data is available ...
      groundlevel[i] <- min(c(median(driver[grid_idx]),  d[time_idx(t, groundtime[i])$idx]))
    }
  }

  tonicDriver <- pracma::pchip(groundtime, groundlevel, t)
  groundtime_pre <- groundtime
  groundlevel_pre <- groundlevel
  tonicDriver_pre <- tonicDriver
  # browser()
  tonicData <- pracma::conv(c(tonicDriver[1]*rep(1,nKernel), tonicDriver), kernel)
  tonicData <- tonicData[seq(nKernel,(length(tonicData)-nKernel), by=1)]
  tonicData_pre <- tonicData

  # Correction for tonic sections still higher than raw data
  # Move closest groundtime at time of maximum difference of tonic surpassing data
  # browser()
  for (i in seq(length(groundtime)-1, 1, by = -1)){

    t_idx <- subrange_idx(t, groundtime[i], groundtime[i+1])
    ddd <- max((tonicData[t_idx]+leda$set$dist0_min) - d[t_idx])
    idx <- which.max((tonicData[t_idx]+leda$set$dist0_min) - d[t_idx])

    if (ddd > eps(1)){
      # Move closest groundtime to maxmimum difference position and level
      groundlevel[i] <- groundlevel[i] - ddd
      groundlevel[i+1] <- groundlevel[i+1] - ddd

      tonicDriver <- pracma::pchip(groundtime, groundlevel, t)
      tonicData <- pracma::conv(c(tonicDriver[1]*rep(1,nKernel), tonicDriver), kernel)
      tonicData <- tonicData[seq(nKernel,(length(tonicData)-nKernel), by=1)]
    }
  }
  pp <- pracma::pchipfun(groundtime, groundlevel)

  # browser()
  # Save to vars
  leda$analysis0$target$poly <- pp
  leda$analysis0$target$groundtime <- groundtime
  leda$analysis0$target$groundlevel <- groundlevel
  leda$analysis0$target$groundlevel_pre <- groundlevel_pre

  leda$analysis0$target$iif_t <- iif_t
  leda$analysis0$target$iif_data <- iif_data

  # Plot tonic fit
  # TODO add these plots in some other plotting function later

  # if (0){
  #   figure;
  #   plot(t, d,'k')
  #   hold on;
  #   plot(t, driver,'k')
  #   plot(iif_t,iif_data,'.','Color',[.5 .5 .5])
  #
  #   plot(groundtime_pre, groundlevel_pre,'bo')
  #   plot(t, tonicDriver_pre,'b:')
  #
  #   plot(groundtime, groundlevel,'go')
  #   plot(t, tonicDriver,'g')
  #
  #   plot(t, tonicData_pre,'r');
  #   plot(t, tonicData,'m')
  #
  #
  #   legend('Data','Driver','Inter-impulse data','Groundlevel-pre','TonicDriver-pre','Groundlevel','TonicDriver','TonicData-pre','TonicData')
  # }

  output <- list(leda = leda,
                 tonicDriver = tonicDriver,
                 tonicData = tonicData)
  # print("completed sdeco_interimpulsefit")
  return(output)
}


#' @title
#'
#' @param data
#' @param fit
#' @param npars number of unfree parameters with df <- n - npar
#' @param errortype
#' @param err_data The conductance error that is saved in leda$data$conductance$error. This input will only be passed when errortype <- "Chi2"
#'
#' @author Nidhi Desai
#'
fiterror <- function(data, fit, npar, errortype, err_data = 0){

  residual <- data - fit
  n <- length(data)
  df <- n - npar
  SSE <- sum(residual^2)

  if (errortype == "adjR2"){  # adjusted-R^2; for optimization use 1 - adjR2 since you want to minimize the function
    SST <- sd(data) * n
    r2 <- 1 - SSE/SST
    error <- 1-(1-r2)*(n-1)/df
  }
  if (errortype == "MSE"){
    error <- SSE/n #MSE, non-normalized
  }
  if (errortype == "RMSE"){
    error <- sqrt(SSE/n) #RMSE, non-normalized
  }
  if (errortype == "Chi2"){
    error <-  SSE/err_data
  }

  return(error)
}


#' @title
#'
#' @param v
#' @param elim
#'
#' @author Nidhi Desai
#'
deverror <- function(v, elim){
  idx <- (v > 0) & (v < elim)
  err <- 1 + ((sum(v[idx])/elim) - sum(idx)) / length(v)
  return(err)
}


#' @title
#'
#' @description succnz calculates an index of how many successive values are above the
#'   parameter crit as described in section 4.3 of Benedek, M. & Kaernbach, C. (2010).
#'   A continuous measure of phasic electrodermal activity. J. Neurosci. Methods, 190, 80–91.
#' @param data
#' @param crit
#' @param fac
#' @param sr
#' @return
#'
#' @author Nidhi Desai
#'
succnz <- function(data, crit, fac, sr){

  n <- length(data)
  abovecrit <- as.numeric(abs(data) > crit)
  nzidx <- which(diff(abovecrit) != 0) + 1

  if (length(nzidx) == 0){
    snz <- 0
    return(snz)
  }

  # if the sequence begins with a value above crit prepend 1
  if (abovecrit[1] == 1){ # if true
    nzidx <- c(1, nzidx)
  }
  # if the sequence ends with a value above crit append the length
  if (abovecrit[length(abovecrit)] == 1){ # if true
    nzidx <- c(nzidx, n+1)
  }

  # now nzidx contains every position where data rises above crit (odd indices) or dips below crit (even indices).
  # The lengths of spans above crit is the difference between the start index and the end index
  nzL <- nzidx[seq(2, length(nzidx), by = 2)] - nzidx[seq(1, length(nzidx), by = 2)]

  snz <- sum((nzL/sr)^fac) / (n/sr)

  return(snz)
}


#' @title
#'
#' @param start_val
#' @param leda
#' @param error_fcn
#' @param h
#' @param crit_error
#' @param crit_iter
#' @param crit_h
#'
#' @author Nidhi Desai
#'
cgd <- function(start_val, leda, error_fcn, h, crit_error, crit_iter, crit_h){

  x <- start_val
  error_fcn_output <- error_fcn(x, leda)
  newerror <- error_fcn_output$err
  leda <- error_fcn_output$leda # ADDED THIS to output leda
  # print("first round error_fcn inside cgd")
  starterror <- newerror
  history <- list()
  history$x <- x
  history$direction <- rep(0, length(x))
  history$step <- -1
  history$h <- rep(-1, length(h))
  history$error <- newerror
  iter <- 0

  while (1){
    iter <- iter + 1
    olderror <- newerror
    print(paste("cgd iter:", toString(iter)))

    # GET GRADIENT
    if (iter == 1){
      cgd_get_gradient_ouput <-  cgd_get_gradient(x, olderror, error_fcn, leda, h)
      gradient <- cgd_get_gradient_ouput$gradient
      leda <- cgd_get_gradient_ouput$leda
      direction <- -1*gradient
      if (length(gradient) == 0){ break }
    } else {
      cgd_get_gradient_ouput <- cgd_get_gradient(x, olderror, error_fcn, leda, h)
      new_gradient <- cgd_get_gradient_ouput$gradient
      leda <- cgd_get_gradient_ouput$leda
      old_direction <- direction
      # browser()
      old_gradient <- gradient

      method <- 1 # TODO I dont get why are we setting method to 1 here

      if (method == 1){
        # no conjugation
        direction <- -new_gradient
      } else if (method == 2) {
        # Fletcher-Reeves
        beta <- beta <- norm(new_gradient, type = "2") / norm(old_gradient, type = "2")
        direction <- -new_gradient + beta * old_direction
      } else if (method == 3) {
        # Polak-Ribiere
        a <- (new_gradient - old_gradient) * new_gradient
        b <- old_gradient * old_gradient
        beta <- max(a / b, 0)
        direction <- -new_gradient + beta * old_direction
      } else if (method == 4) {
        # Hestenes-Stiefel
        a <- t((new_gradient - old_gradient) * new_gradient)
        b <- t(old_direction * (new_gradient - old_gradient))
        beta <- a / b
        direction <- -new_gradient + beta * old_direction
      }
    }

    if (any(direction != 0)){ # any(direction)
      # LINESEARCH
      # print("third call start")
      cgd_line_output <- cgd_linesearch(x, olderror, direction, error_fcn, leda, h)
      # print("third call end")
      x <- cgd_line_output$xc
      newerror <- cgd_line_output$error1 # cgd_line_output$newerror
      step <- cgd_line_output$step
      leda <- cgd_line_output$leda
      error_diff <- newerror - olderror
    } else {
      error_diff <- 0 # empty gradient
      step <- 0
    }

    # history
    history$x <- rbind(history$x, x) # history$x[iter+1,] <- x
    history$direction <- rbind(history$direction, direction) # history$direction[iter+1,] <- direction
    history$step[iter+1] <- step
    history$h <- rbind(history$h, h) # history$h[iter+1,] <- h
    history$error[iter+1] <- newerror

    if (iter > crit_iter){
      break
    }

    if (error_diff > -crit_error){ # no improvement
        h <- h/2
        if (all(h < crit_h)){ break }
    }

  }
  # add2log(0,['Optimized parameter: ',sprintf('%5.2f\t',x),sprintf(' Error: %6.3f',newerror),' (Initial parameter: ',sprintf('%5.2f\t',start_val), sprintf(' Error: %6.3f)',starterror)], 0,0,1,1,0)
  return(list(x = x, history = history, leda = leda))
}

#' @title
#'
#' @param x
#' @param error0
#' @param error_fcn
#' @param leda
#' @param h
#'
#' @author Nidhi Desai
#'
cgd_get_gradient <- function(x, error0, error_fcn, leda, h){

  Npars <- length(x)
  gradient <- rep(0, Npars)

  for (i in 1:Npars){
    xc <- x  # x_copy
    xc[i] <- xc[i] + h[i]
    error_fcn_output <- error_fcn(xc, leda)
    error1 <- error_fcn_output$err
    leda <- error_fcn_output$leda

    if (error1 < error0){
      gradient[i] <- error1 - error0
    } else { # try opposite direction
      xc <- x
      xc[i] <- xc[i] - h[i]
      error_fcn_output <- error_fcn(xc, leda)
      error1 <- error_fcn_output$err

      if (error1 < error0){
        gradient[i] <- -(error1 - error0)
      } else {
        gradient[i] <- 0
      }
    }
  }
  gradient <- t(gradient) #gradient = gradient(:)'
  return(list(gradient = gradient, leda = leda))
}

#' @title
#'
#' @param x
#' @param error0
#' @param direction
#' @param error_fcn
#' @param leda
#' @param h
#'
#' @author Nidhi Desai
#'
cgd_linesearch <- function(x, error0, direction, error_fcn, leda, h){

  direction_n <- direction / norm(direction,type = "2")
  error_list <- error0
  factor_cgd <- 0
  stepsize <- h
  maxSteps <- 6

  for (iStep in 2:maxSteps){
    factor_cgd[iStep] <- 2^(iStep-2)
    xc <- x + direction_n * stepsize * factor_cgd[iStep]
    error_fcn_output <- error_fcn(xc, leda) # xc may be changed due to limits
    error_list[iStep] <- error_fcn_output$err
    xc <- error_fcn_output$x
    leda <- error_fcn_output$leda

    if ((error_list[length(error_list)] >= error_list[length(error_list)-1])){ # end of decline
      if (iStep == 2){ # no success
        step <- 0
        error1 <- error0
      } else { # parabolic
        p <- coef(lm(error_list ~ factor_cgd + I(factor_cgd^2))) # polyfit(factor_cgd, error_list, 2)
        fx <- seq(factor_cgd[1], factor_cgd[length(factor_cgd)], by = 0.1)
        # browser()
        fy <- p[[1]] + p[[2]]*fx + p[[3]]*(fx^2)
        # fy <- predict(p, fx) # TODO LEFT HERE 4/28/24
        idx <- which.min(fy)
        fxm <- fx[idx]
        xcm <- x + direction_n * stepsize * fxm
        error_fcn_output = error_fcn(xcm, leda) # xc may be changed due to limits
        error1 <- error_fcn_output$err
        xcm <- error_fcn_output$x
        leda <- error_fcn_output$leda
        if (error1 < error_list[iStep-1]){
          xc <- xcm
          step <- fxm
        } else { # finding Minimum did not work
          xc <- x + direction_n * stepsize * factor_cgd[iStep-1] # before last point
          error_fcn_output <- error_fcn(xc, leda) # recalculate error in order to check for limits again
          error1 <- error_fcn_output$err
          xc <- error_fcn_output$x
          leda <- error_fcn_output$leda
          step <- factor_cgd[iStep-1]
        }
      }
      return(list(xc = xc, error1 = error1, step = step, leda = leda))
    }
  }

  step <- factor_cgd[iStep]
  error1 <- error_list[iStep]

  output <- list(xc = xc, error1 = error1, step = step, leda = leda)
  return(output)
}
