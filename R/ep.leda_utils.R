############################
##### List of subsidiary functions for initialization n ledalab
############################
# - get_peaks()
# - withinlimits()
# - normpdf()
# - refresh_data()
# -- trough2peak_analysis()
# - add2log()
############################

#' @title 
#' 
#' @param 
#' @return 
#' 
#' @author Nidhi Desai
#'
get_peaks <- function(data){

  cccrimin <- c()
  cccrimax <- c()
  ccd <- diff(data) # Differential
  
  # Search for signum changes in first differential:
    # slower but safer method to determine extrema than looking for zeros (taking into account
    # plateaus where ccd does not immediately change the signum at extrema)
  start_idx <- which(ccd != 0)[1]

  if (length(start_idx) == 0){ # data == zeros(1,n)
    output <- list(cccrimin = cccrimin, 
                    cccrimax = cccrimax)
    return(output)
  }

  cccri <- rep(0, length(ccd)) # zeros(1, length(ccd), 'uint32');
  cccriidx <- 2
  csi <- sign(ccd[start_idx]) # currentsignum = current slope
  signvec <- sign(ccd)
  for (i in seq(start_idx+1, length(ccd), by = 1)){
    if (signvec[i] != csi){
      cccri[cccriidx] <- i
      cccriidx <- cccriidx + 1
      csi <- -csi
    }
  }
  
  if (cccriidx == 2){ # no peak as data is increasing only
    output <- list(cccrimin = cccrimin, 
                   cccrimax = cccrimax)
    return(output)
  }
  
  # if first extrema = maximum, insert minimum before
  if (sign(ccd[start_idx]) == 1){
    predataidx <- c(start_idx:(cccri[2] - 1))
    mn <- min(data[predataidx])
    idx <- which.min(data[predataidx])
    cccri[1] <- predataidx[idx]
  }
  
  # if last extremum is maximum add minimum after it
  if ((cccriidx - (cccri[1] == 0)) %% 2){
    cccri[cccriidx] <- length(data)
    cccriidx <- cccriidx + 1
  }
  
  # crop cccri from the first minimum to the last written index
  cccri <- cccri[(1+(cccri[1]==0)) : (cccriidx-1)]
  cccri <- sort(cccri)
  
  cccrimin <- cccri[seq(1, length(cccri), by = 2)]  # list of minima
  cccrimax <- cccri[seq(2, length(cccri), by = 2)]  # list of maxima
  
  output <- list(cccrimin = cccrimin, 
                  cccrimax = cccrimax)
  return(output)
}

#' @title 
#' 
#' @param 
#' @return 
#' 
#' @author Nidhi Desai
#'
withinlimits <- function(w_in, lowerlimit, upperlimit){
  w_out <- max(min(w_in, upperlimit),lowerlimit)
  return(w_out)
}


#' @title 
#' 
#' @param x data point at which we are calculating the value of the normal distribution with mean 'mu' 
#' @param mu mean of the normal distribution
#' @param sigma standard deviation of the normal distribution
#' @return 
#' 
#' @author Nidhi Desai
#'
normpdf <- function(x, mu, sigma){ # using this function instead of dnorm in R in smooth function
  y <- exp(-0.5 * ((x - mu)/sigma)^2) / (sqrt(2*pi)* sigma)
  return(y)
}


#' @title
#' 
#' @param
#' 
#' @return
#' 
#' @author Nidhi Desai
#' 
refresh_data <- function(time_ts, eda_ts, Hz){
  refreshed_leda <- list()
  
  # Data statistics
  refreshed_leda$N <- length(eda_ts)
  refreshed_leda$Hz <- (refreshed_leda$N - 1) / (time_ts[length(time_ts)] - time_ts[1])
  refreshed_leda$eda_min <- min(eda_ts)
  refreshed_leda$eda_max <- max(eda_ts)
  refreshed_leda$eda_error <- sqrt(mean(diff(eda_ts)^2)/2)

  output <- smooth_adapt(eda_ts, 'gauss', 1 * Hz, .00001)
  refreshed_leda$time_ts <- time_ts 
  refreshed_leda$eda_smooth_data <- output$scs
  refreshed_leda$eda_smooth_data_win <- output$winwidth
  
  refreshed_leda <- trough2peak_analysis(refreshed_leda)
  
  return(refreshed_leda)
}

#' @title Trough to peak analysis
#' 
#' @param refreshed_data 
#'
#' @author Nidhi Desai
#'
trough2peak_analysis <- function(refreshed_data){
  
  ds <- refreshed_data$eda_smooth_data
  t <- refreshed_data$time_ts

  output <- get_peaks(ds)
  minL <- output$cccrimin
  maxL <- output$cccrimax
  
  minL <- minL[1:length(maxL)]
  
  refreshed_data$trough2peakAnalysis <- list()
  refreshed_data$trough2peakAnalysis$onset = t[minL]
  refreshed_data$trough2peakAnalysis$peaktime = t[maxL]
  refreshed_data$trough2peakAnalysis$onset_idx = minL
  refreshed_data$trough2peakAnalysis$peaktime_idx = maxL
  refreshed_data$trough2peakAnalysis$amp = ds[maxL] - ds[minL]
  
  return(refreshed_data)
}




