############################
##### List of subsidiary functions for initialization n ledalab
############################
# - get_peaks()
# - withinlimits()
# - normpdf()
# - refresh_data()
# -- trough2peak_analysis()
# - add2log()
# - divisors()
# - subrange_idx()
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

  if ((length(start_idx) == 0) | is.na(start_idx)){ # data == zeros(1,n)
    output <- list(cccrimin = cccrimin,
                    cccrimax = cccrimax)
    return(output)
  }

  cccri <- rep(0, length(ccd)) # zeros(1, length(ccd), 'uint32');
  cccriidx <- 2
  csi <- sign(ccd[start_idx]) # currentsignum <- current slope
  signvec <- sign(ccd)

  for (i in (start_idx+1):length(ccd)){
    if (signvec[i] != csi){
      cccri[cccriidx] <- i
      cccriidx <- cccriidx + 1
      csi <- -csi
    }
  }

  if (cccriidx == 2){ # no peak as data is increasing only
    output <- list(min = cccrimin,
                   max = cccrimax)
    return(output)
  }

  # if first extrema <- maximum, insert minimum before
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

  output <- list(min = cccrimin,
                  max = cccrimax)
  return(output)
}


#' @title time index function
#'
#' @param time
#' @param time0
#'
#' @author Nidhi Desai
#'
time_idx <- function(time, time0){

  idx <- which(time >= time0)
  if (length(idx) > 0){
    idx <- min(idx)
    time0_adj <- time[idx]

    # check if there is a closer idex before
    if (time0_adj != time[1]){
      time0_adjbefore <- time[idx-1]
      if (abs(time0 - time0_adjbefore) < abs(time0 - time0_adj)){
        idx = idx - 1
        time0_adj = time0_adjbefore
      }
    }
  } else {
    idx <- find(time <= time0)
    idx <- max(idx)
    time0_adj <- time(idx)
  }

  return(list(idx = idx, time0_adj = time0_adj))
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
#' @param leda
#'
#' @author Nidhi Desai
#'
trough2peak_analysis <- function(leda){

  if (length(leda$smoothed) > 0) {
    ds <- leda$smoothed$eda_smooth_data
    t <- leda$smoothed$time_ts
  } else if (length(leda$filtered) > 0){
    ds <- leda$filtered$eda_smooth_data
    t <- leda$filtered$time_ts
  } else {
    ds <- leda$updated$eda_ts
    t <- leda$updated$time_ts
  }

  output <- get_peaks(ds)
  minL <- output$min
  maxL <- output$max

  minL <- minL[1:length(maxL)]

  leda$trough2peakAnalysis <- list()
  leda$trough2peakAnalysis$onset <- t[minL]
  leda$trough2peakAnalysis$peaktime <- t[maxL]
  leda$trough2peakAnalysis$onset_idx <- minL
  leda$trough2peakAnalysis$peaktime_idx <- maxL
  leda$trough2peakAnalysis$amp <- ds[maxL] - ds[minL]

  return(leda)
}


#' @title find all divisors of input value
#'
#' @param input A value for which all divisors should be found
#'
#' @author Nidhi Desai
#'
divisors <- function(input){

  ps <- powerset(matlab::factors(input))

  d <- rep(0, length(ps))
  for (i in 1:length(ps)){
    d[i] <- prod(ps[[i]])
  }
  d <- c(1, unique(d)) # all divisors
  d <- d[2:(length(d)-1)] # non trivial divisors

  return(d)
}


#' @title title list of all combinations of input vector
#'
#' @param v list of prime factors
#'
#' @author Nidhi Desai
#'
powerset <- function(v){
  ps <- list()
  for (i in 1:length(v)){
    s <- t(utils::combn(v,i)) # Generates all combinations of v elements taken i at a time
    for (j in 1:nrow(s)){
      # print(s[j,])
      ps <- c(ps, list(s[j,]))
    }
  }
  return (ps)
}


#' @title
#'
#' @param t1
#' @param t2
#' @param time_ts
#' @param eda_ts
#'
#' @author Nidhi Desai
#'
subrange <- function(t1, t2, time_ts, eda_ts){

  t_idx <- which(time_ts >= t1 & time_ts <= t2)
  ts <- time_ts[t_idx]
  cs <- eda_ts[t_idx]

  return(list(ts = ts, cs = cs, t_idx = t_idx))
}


#' @title
#'
#' @param t
#' @param t1
#' @param t2
#'
#' @author Nidhi Desai
#'
subrange_idx <- function(t, t1, t2){
  t1_idx <- time_idx(t, t1)$idx
  t2_idx <- time_idx(t, t2)$idx
  if ((length(t1_idx) > 0) & (length(t2_idx) > 0)){
    idx <- t1_idx:t2_idx
  } else {
      idx <- c()
  }
  return(idx)
}


#'  @title Plot decomposed phasic and tonic data
#'
#'  @param leda
#'  @param open_plot 1 to plot the figure and provide as output, 0 to not plot the figure but provide the ggarange object as output
#'
#'  @author Nidhi Desai
#'
plot_decomposed_data <- function(leda, open_plot = 1){
  leda_df <- as.data.frame(leda$updated)
  leda_df$tonicData <- leda$analysis$tonicData
  leda_df$phasicData <- leda$analysis$phasicData
  leda_df$driver <- leda$analysis$driver
  leda_df$remainder <- leda$analysis$remainder # TODO remainder has wierd values

  g1 <- ggplot(leda_df, aes(x = time_ts)) +
        geom_line(aes(y = eda_ts, color = "EDA")) +
        geom_line(aes(y = tonicData, color = "Tonic Data")) +
        scale_color_manual(values = c("EDA" = "black", "Tonic Data" = "red")) +
        labs(color = "") + theme_minimal() + ggtitle('SC and Tonic Data') +
        theme(legend.position = c(0.95, 1),  # Move the legend to the top right corner
             legend.justification = c(1.1, 0.7))

  g2 <- ggplot(leda_df, aes(x = time_ts)) +
    geom_line(aes(x = time_ts, y = driver, color = "Phasic Driver")) +
    scale_color_manual(values = c("Phasic Driver" = "darkgray")) +
    theme_minimal() + theme(legend.position = "none") + ggtitle('Phasic Driver')

  gtotal <- ggarrange(g1, g2, nrow = 2, align = 'v')

  if (open_plot == 1){
    print({gtotal})
  }
  return(gtotal)
}


