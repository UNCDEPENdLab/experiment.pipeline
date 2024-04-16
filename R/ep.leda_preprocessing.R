############################
##### List of subsidiary functions use for preprocessing EDA data in ledalab
############################
# - leda.filter()
# - leda.smoothing()
# -- smooth_non_adaptive()
# -- smooth_adapt_wrapper()
# --- smooth_adapt()
# ---- smooth()
############################

#' @title low-pass filtering
#' @description Apply a low-pass Butterworth filter with given settings, 
#' @example leda.filter(eda, 1000, c(1,5)) for a 1rst order low-pass filter with 5Hz cutoff
#' @param eda_ts a vector containing the EDA signal. In experiment.pipeline objects, this vector can be found under ep.physio$raw$<eda signal name>
#' @param Hz the sampling rate (in Hz) of the raw ECG signal. Defaults to 1000 Hz
#' @param filter_opts filter inputs: filter order - filter_opts$filter_order and low cutoff frequency - filter_opts$minFreq
#' @return TODO add here
#' 
#' @author Nidhi Desai
#'
#' @export
#'
leda.filter <- function(time_ts, eda_ts, Hz, filter_opts){
  filter_order <- filter_opts$filter_order
  low_cutoff_freq <- filter_opts$minFreq
  nyquist_freq <- Hz/2 # Nyquist frequency
  Wn <- low_cutoff_freq/nyquist_freq;    # non-dimensional frequency
  #gsignal
  bf <- signal::butter(filter_order, Wn, "low") # using signal package now instead of gsignal since it is a direct translation of matlab pacakge. construct the filter, https://search.r-project.org/CRAN/refmans/gsignal/html/butter.html
  filtered_signal <- signal::filtfilt(bf, eda_ts); # filter the data with zero phase, https://search.r-project.org/CRAN/refmans/gsignal/html/filtfilt.html
  
  leda_filter_output <- refresh_data(time_ts, filtered_signal, Hz)
  leda_filter_output$time_ts <- time_ts
  leda_filter_output$eda_ts <- filtered_signal
  
  # add2log(1,['Data filtered with ',  filterTypeL{typenr},' (order: ', num2str(order),', lower-cutoff: ',num2str(lo_cutoff_freq),')'],1,1,1); # TODO add this after add2log function is done
  return(leda_filter_output)
}

#' @title perform data smoothing
#' 
#' @param time_ts a vector containing the time signal.
#' @param eda_ts a vector containing the EDA signal.
#' @param Hz the sampling rate (in Hz) of the raw ECG signal.
#' @param type mean (moving average), hann (hanning window), gauss (gauss window), adapt (adaptive smoothing using gauss).
#' @param width width of smoothing window in samples (does not apply for adapt).
#' 
#' @return TODO add here
#' 
#' @author Nidhi Desai
#' 
#' @export
#' 
leda.smoothing <- function(time_ts, eda_ts, Hz, type, width){
  if (type == "adapt"){
    smooth_adapt_wrapper(time_ts, eda_ts, Hz) # adaptive smoothing using gauss
  } else {
    smooth_non_adaptive(eda_ts, width, type) # could be moving average, hanning window, gauss window smoothing
  }
}

#' @title adaptive smoothing
#' 
#' @author Nidhi Desai
#' 
smooth_adapt_wrapper <- function(time_ts, eda_ts, Hz){ # this function is named as adaptive_smoothing in ledalab matlab
  
  refreshed_data <- refresh_data(time_ts, eda_ts, Hz)
  winwidth_max <-  refreshed_data$Hz * 3
  # plot(time_ts, eda_ts, ylim = c(9.8, 11.2)) # These plots look same with matlab at this point in the code
  # print("inside adaptive smoothing")
  smooth_adapt_output <- smooth_adapt(eda_ts, 'gauss', winwidth_max, .00003)
  scs <- smooth_adapt_output$scs
  winwidth <- smooth_adapt_output$winwidth

  if (winwidth == winwidth_max){
    print("Warning: Data could not meet smoothness criteria for adaptive smoothing")
    # add2log()
    return(NA)
  } else if (winwidth== 0){
    print("Data already satisfy smoothness criteria for adaptive smoothing")
    return(NA)
    # add2log()
  } else {
    smoothed_signal <- scs # scs(:)'; transpose scs if needed
    leda_smooth_output <- refresh_data(time_ts, smoothed_signal, Hz)
    leda_smooth_output$time_ts <- time_ts
    leda_smooth_output$eda_ts <- smoothed_signal
    
    # add2log(1,['Adaptive data smoothing applied (',num2str(winwidth),' samples gauss window)'],1,1,1)
    
    print("Completed adaptive smoothing")
    return(leda_smooth_output)
  }
}

#' @title smooth adapt
#' 
#' @author Nidhi Desai
#' 
smooth_adapt <- function(eda_ts, type, winwidth_max, err_crit){ # smooth_adapt.m function in matlab ledalab
  # type in adaptive smoothing will always be 'gauss' 
  success <- 0
  # print(paste("winwindth_max:", winwidth_max))
  
  ce <- sqrt(mean(diff(eda_ts)^2)/2) # LEFT HERE this value is 0.0014 in matlab and 0.0267 in R
  iterL <- seq(0, winwidth_max, 4)
  # print(iterL)
  if (length(iterL) < 2){ iterL <- c(0, 2) }
  reach_err_crit <- 0

  # finding a value of iterL where the conductance error different goes below a critical value err_crit
  for (i in c(2:length(iterL))){
    winwidth <- iterL[i]
    # print(paste("winwidth:", winwidth))
    scs <- smooth(eda_ts, winwidth, type)
    scd <- diff(scs)
    ce <- c(ce, sqrt(mean(scd^2)/2)) # conductance_error
    # print(toString(ce[length(ce)]))
    # print(abs(ce[i] - ce[i-1]))
    # print(err_crit)
    if (abs(ce[i] - ce[i-1]) < err_crit){ # if difference in successive conductance error is less than error criteria
      reach_err_crit <- i
      success = 1
      # print("crossed err_crit")
      break
    }
  }
  # print("Success:")
  # print(success)
  if (success){ # take before-last result
    if (reach_err_crit > 2) {
      scs <- smooth(eda_ts, iterL[i-1], type)
      winwidth <- iterL[i-1]
    } else { # data already satisfy smoothness criteria
      scs <- eda_ts
      winwidth <- 0
    }
  }
  
  output <- list()
  output$scs <- scs
  output$winwidth <- winwidth
  
  # add2log(1,['Adaptive data smoothing applied (',num2str(winwidth),' samples gauss window)'],1,1,1);

  return(output)
}

#' @title basic function for smoothing data for different types
#' 
#' @author Nidhi Desai
#'
smooth <- function(eda_ts, winwidth, type = 'gauss'){
  
  if (winwidth < 1){
    sdata <- eda_ts
    return(sdata)
  }
  
  data <- c(eda_ts[1], eda_ts, eda_ts[length(eda_ts)]) # pad to remove border errors
  winwidth <- floor(winwidth/2)*2  # force even winsize for odd window

  window <- case_when(type == "hann" ~ 0.5*(1 - cos(2*pi*(seq(0,1,1/winwidth)))), # hanning window
                      type == "mean" ~ rep(1, winwidth+1), # moving average
                      type == "gauss" ~ normpdf(1:(winwidth+1), (winwidth/2)+1, winwidth/8), # probability density function (pdf) of the standard normal distribution
                      type == "expl" ~ c(rep(0, winwidth/2), exp(-4*(seq(0, 1, 2/winwidth)))),
                      TRUE ~ NA)
  
  # print("inside window smooth")
  # print("Window:")
  # print(toString(round(window, 4)))
  # Till here is same as matlab
  
  if (sum(is.na(window)) > 0){ stop("Unknown type") }
  window <- window / sum(window)  # normalize window

  data_ext <- c(rep(1, winwidth/2)*data[1], data, rep(1, winwidth/2)*data[length(data)]) # extend data to reduce convolution error at beginning and end
  # print(toString(round(data_ext[1:10],3)))
  sdata_ext <- mSTEM::conv(data_ext, window) # convolute with window
  # replaced pracma conv function with mSTEM conv function as it is a more exact match of matlab conv function
  # print(toString(round(sdata_ext[1:10],3)))
  sdata <- sdata_ext [(2+winwidth):(length(sdata_ext)-winwidth-1)] # cut to data length
  return (sdata)
}


#' @title non-adaptive types of smoothing
#' 
#' @author Nidhi Desai
#'
smooth_non_adaptive <- function(eda_ts, width, type){ # matches the smooth_data.m function in matlab ledalab
  
  smoothed_signal <- smooth(eda_ts, width, type) # transpose scs if needed
  # downsampling (type factor mean) may result in an additional offset = time(1), which will not be substracted (tim = time - offset) in order not to affect event times

  leda_smooth_output <- refresh_data(time_ts, smoothed_signal, Hz)
  leda_smooth_output$time_ts <- time_ts
  leda_smooth_output$eda_ts <- smoothed_signal

  return(leda_smooth_output)
  # smoothWinL <- c('hann window','moving average','gauss window')
  # typenr <- which(type %in% c('hann', 'mean', 'gauss'))
  # add2log(1,['Data smoothed with ',  smoothWinL{typenr},' (',num2str(width), ' samples width)'],1,1,1);
  
}


