############################
##### List of subsidiary functions use for performing CDA analysis in ledalab
############################
# - leda.sdeco()
############################

#' @description Performs stochastic deconvolution to gain back the original signals that were convoluted
#'
leda.sdeco <- function(leda){ # this functions calls other functions in ep.leda_deconvolution.R

  nr_iv <- leda$opts$optimize$nr_iv

  if (!is.numeric(nr_iv)){ stop("Optimize option requires numeric argument (# of initial values for optimization)") }

  leda$set$dist0_min <- 0
  leda$set$segmWidth <- 12

  # Downsample data for preanalysis, downsample if N > N_max but keep samplingrate at 4 Hz minimum
  leda$analysis0$target <- c()
  leda$analysis0$target$t <- leda$updated$time_ts
  leda$analysis0$target$d <- leda$updated$eda_ts
  leda$analysis0$target$sr <- leda$Hz

  Fs_min <- 4
  N_max <- 3000
  Fs <- round(leda$Hz)
  N <- length(leda$updated$eda_ts)

  # downsampling
  if (N > N_max){
    print("Downsampling further for CDA analysis")
    # if Fs==20, factorL <- c(2, 5, 4, 10)
    factorL <- sort(divisors(Fs))
    FsL <- Fs / factorL
    idx <- which(FsL >= Fs_min)
    factorL <- factorL[idx]
    FsL <- FsL[idx]
    leda$analysis0$downsampl$Hz <- factorL

    if (length(FsL) > 0){
      N_new <- N / factorL
      idx <- which(N_new < N_max)
      if (length(idx) > 0){ # ~isempty(idx)
        idx <- idx[1]
      } else {
        idx <- length(factorL) # if no factor meets criterium, take largest factor
      }
      fac <- factorL[idx]
      downsampled <- downsamp(leda$updated$time_ts, leda$updated$eda_ts, fac, 'step')
      leda$analysis0$target$t <- downsampled$time_ts
      # leda$updated$time_ts <- downsampled$time_ts
      leda$analysis0$target$d <- downsampled$eda_ts
      # leda$updated$eda_ts <- downsampled$eda_ts
      leda$analysis0$target$sr <- FsL[idx]
      leda$analysis0$downsampl$Hz <- factorL[idx]
    } else {
      print("No need for downsampling further")
    }
  }

  leda$analysis0$tau <- leda$set$tau0_sdeco
  leda$analysis0$smoothwin <- leda$set$smoothwin_sdeco # sec
  leda$analysis0$tonicGridSize <- leda$set$tonicGridSize_sdeco

  sdeconv_output <- sdeconv_analysis(leda$analysis0$tau, leda) # set dist0
  err <- sdeconv_output$err
  x <- sdeconv_output$x
  leda <- sdeconv_output$leda

  optim_output <- deconv_optimize(x, nr_iv, leda)
  leda <- optim_output$leda

  # leda <- deconv_apply(leda)

  return(leda)
}


