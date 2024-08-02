############################
##### List of subsidiary functions for initialization n ledalab
############################
# - leda.initialization()
# -- leda.preset()
############################

#' @title initializes the leda data object
#' @description this function initializes the leda object by adding raw data,
#'   extracting leda processing setting options from config file and setting
#'   other variables to preset values.
#' @param ep.physio
#' @param phys_config
#' @return leda object initialized with raw data and variables set with values from config file for ledalab proessing
#'
#' @author Nidhi Desai
#'
#' @export

leda.initialization <- function(ep.physio, phys_config){

  # 1.1 setup leda data structure and default parameter values
  leda <- leda.preset(ep.physio, leda)

  # 1.2 Extract relevant ledalab options from config file
  # TODO get these values from ep.physio and config file and add to leda structure below

  ## Filtering
  if (!is.null(phys_config$eda_preproc$filtering$filter_order)) {
    leda$opts$filter$filter_order <- phys_config$eda_preproc$filtering$filter_order
  }
  if (!is.null(phys_config$eda_preproc$filtering$minFreq)) {
    leda$opts$filter$minFreq <- phys_config$eda_preproc$filtering$min_freq
  }

  ## Smoothing
  if (!is.null(phys_config$eda_preproc$smoothing$type)) {
    leda$opts$smooth$type <- phys_config$eda_preproc$smoothing$type
  }
  if (!is.null(phys_config$eda_preproc$smoothing$width)) {
    leda$opts$smooth$width <- phys_config$eda_preproc$smoothing$width
  }

  ## Decomposition
  if (!is.null(phys_config$eda_preproc$decomposition$scr$scr_amp_threshold)) {
    leda$set$export$SCRmin_scr_output <- phys_config$eda_preproc$decomposition$scr$scr_amp_threshold
  }
  if (!is.null(phys_config$eda_preproc$decomposition$scr$era_amp_threshold)) {
    leda$set$export$SCRmin_era_output <- phys_config$eda_preproc$decomposition$scr$era_amp_threshold
  }
  if (!is.na(phys_config$eda_preproc$decomposition$scr$response_window[1])) {
    leda$set$export$SCRstart <- phys_config$eda_preproc$decomposition$scr$response_window[1]
    leda$set$export$SCRend <- phys_config$eda_preproc$decomposition$scr$response_window[2]
  }

  ## Optimization
  if (!is.null(phys_config$eda_preproc$optimizing$num_start_values)){
    leda$opts$optimize$nr_iv <- phys_config$eda_preproc$optimizing$num_start_values
  }

  ## SCR detection
  if(!is.null(phys_config$eda_preproc$decomposition$scr)){
    if (!is.null(phys_config$eda_preproc$decomposition$scr$scr_amp_threshold)){
      leda$set$export$SCRmin_scr_output <- phys_config$eda_preproc$decomposition$scr$scr_amp_threshold
    }
    if (!is.null(phys_config$eda_preproc$decomposition$scr$era_amp_threshold)){
      leda$set$export$SCRmin_era_output <- phys_config$eda_preproc$decomposition$scr$era_amp_threshold
    }
    if (!is.na(phys_config$eda_preproc$decomposition$scr$response_window[1])){
      leda$set$export$SCRstart <- phys_config$eda_preproc$decomposition$scr$response_window[1]
      leda$set$export$SCRend <- phys_config$eda_preproc$decomposition$scr$response_window[2]
    }
  }
  return(leda)
}


#' @title
#' @description
#'
#' @param ep.physio
#' @param leda
#'
#' @author Nidhi Desai
#'
leda.preset <- function(ep.physio, leda){

  leda <- list(raw = data.frame(time_ts = ep.physio$raw %>% pull("time_s"),
                                eda_ts = ep.physio$raw %>% select(which(grepl("EDA", names(ep.physio$raw)))) %>% pull(1)),
               Hz = ep.physio$sampling_rate,
               opts = list(filter = list(filter_order = 1, minFreq = 5), # 1st order low-pass filter with 5Hz cutoff
                           smooth = list(type = "adapt", width = 10),
                           optimize = list(nr_iv = 2)),
               intern = list(), # ledalab internal variables
               set = list(), # default settings
               analysis0 = list(),
               analysis = list(),
               events_data = list(event = ep.physio$raw %>% filter(ttl_onset != 0) %>% select(time_s, ttl_onset) %>%
                                         rename(time = time_s) %>% rename(nid = ttl_onset) %>% mutate(name = as.character(nid))),
               pref = list())

  # internal ledalab variables
  leda$intern$install_dir <- "" # TODO figure out how to fill this value
  leda$intern$sessionlog <- list()
  leda$intern$prevfile <- c()
  leda$intern$prompt <- 1

  leda$events_data$N <- nrow(leda$events_data$event)

  # ---- Default Setting ----

  # LEDASET
  # SDECO
  leda$set$tonicGridSize_sdeco <- 10
  leda$set$tau0_sdeco <- c(1, 3.75)  # see Benedek & Kaernbach, 2010, J Neurosc Meth
  leda$set$d0Autoupdate_sdeco <- 0
  leda$set$smoothwin_sdeco <- 0.2
  leda$set$sigPeak <- 0.001

  # get peaks
  leda$set$initVal$hannWinWidth <- 0.5
  leda$set$initVal$signHeight <- 0.01
  leda$set$initVal$groundInterp <- 'spline' #'pchip' keeps only S(x)' continuous
  leda$set$tauMin <- 0.001
  leda$set$tauMax <- 100
  leda$set$tauMinDiff <- 0.01
  leda$set$dist0_min <- 0.001

  # Export (ERA)
  leda$set$export <- list()
  leda$set$export$SCRstart <- 1.00 # sec
  leda$set$export$SCRend   <- 4.00 # sec
  leda$set$export$SCRmin_era_output   <- 0.01 # muS
  leda$set$export$SCRmin_scr_output   <- 0.01 # muS
  leda$set$export$zscale <- 0

  # settings for leda_split()
  leda$set$split <- list()
  leda$set$split$start <- -1   # sec
  leda$set$split$end <- 5       # sec
  leda$set$split$variables <- c('driver','phasicData') # possible variables, 2012-03-13 only one by now$
  leda$set$split$var <- 1       # index for VARIABLES
  leda$set$split$stderr <- 0

  # Ledapref
  leda$pref$showSmoothData <- 0
  leda$pref$showMinMax <- 0
  leda$pref$showOvershoot <- 1
  #not settable inside of Ledalab
  leda$pref$eventWindow <- c(5, 15)
  leda$pref$oldfile_maxn <- 5
  leda$pref$scalewidth_min <- 0.6 # muS

  # Warnings for empty inputs
  if (is.null(leda$Hz)){
    warning("No sampling rate found in ep.physio. Defaulting to 1000 Hz")
  }


  return(leda)
}

