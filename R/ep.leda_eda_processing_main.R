
# ledalab functions should not assume ep.physio data structure.
# The inputs to this function will be an eda signal, time_signal, required input variable values from config file but not the complete config
pacman::p_load(signal, ggplot2, pracma, yaml, checkmate, matlab, tidyverse, ggpubr)
# load("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.physio_dev/ND_local/test_data/acq_data_splice.RData")
load("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/ND_local/acq_data_downsampled_VM_402.RData") # downsampled data
# load("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/ND_local/acq_data_downsampled_VM_667.RData") # downsampled data
load("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/ND_local/acq_data_downsampled_VM_775.RData") # downsampled data
ep.physio <- acq_down # sampling rate of data collected was 2000 Hz but was downsampled to 20 Hz


# saving acq data as txt file to open in MATLAB ledalab for testing
txt_filename <- paste0("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/ND_local/downsampled_VM_775.txt")
txt_df <- ep.physio$raw %>% select(time_s, EDA...EDA..X..PPGED.R, ttl_onset) %>% rename(eda = EDA...EDA..X..PPGED.R)
write.table(txt_df, file = txt_filename, sep = "\t", row.names = FALSE, col.names = FALSE)

config_path <- "C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.physio_dev/inst/extdata/ep_configs/neigborhood/neighborhood_phys.yaml"
source("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/R/validate_exp_yaml.R")
config <- validate_exp_yaml(config_path)
phys_config <- config$definitions$phys

setwd("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/R")
map(c("ep.leda_initialization.R", "ep.leda_preprocessing.R", "ep.leda_utils.R",
      "ep.leda_cda_analysis.R", "ep.leda_deconvolution.R", "ep.leda_deconvolution_helpers.R",
      "ep.leda_bateman.R"), source)

#' @title
#' @description
#'
#' @param ep.physio
#' @param phys_config
#'
#' @author Nidhi Desai
#'
ledalab_eda_main <- function(ep.physio, phys_config){

  # round(leda$raw$eda_ts[100:110], 4) # Raw eda_ts are matching with ledalab

  # -------- 1. initialization ---------
  leda <- leda.initialization(ep.physio, phys_config)

  # TODO need to figure out how to offset time for ledalab
  # issue arising in groundtime variable in sdeco_interimpulsefit function
  time_offset <-  leda$raw$time_ts[1]
  if (time_offset != 0){ leda$raw$time_ts <- leda$raw$time_ts - time_offset }
  if (time_offset != 0){ leda$events_data$event$time <- leda$events_data$event$time - time_offset }

  # -------- 2. preprocessing ----------

  # 2.1. low-pass Butterworth filter
  # higher filter order causes smoother filtering.
  # lower minFreq leads to more aggressive filtering meaning minFreq
  # means that freq higher than this will be filtered out.

  .# leda$opts$filter <- list(filter_order = 1, minFreq = 5)
  # high-frequency noise components are normally eliminated through the use of low-pass filtering
  leda$filtered <- leda.filter(leda$raw$time_ts, leda$raw$eda_ts, leda$Hz, leda$opts$filter)
  leda$updated$time_ts <- leda$filtered$time_ts
  leda$updated$eda_ts <- leda$filtered$eda_ts

  # testing filtering
    # Plotting filtered signal
    filtered_plot <- data.frame(time_ts = leda$filtered$time_ts, eda_ts = leda$filtered$eda_ts)
    raw_plot <- data.frame(time_ts = leda$raw$time_ts, eda_ts = leda$raw$eda_ts)
    ggplot() +
      geom_line(data = filtered_plot, aes(x = time_ts, y = eda_ts), color = "blue") +
      geom_line(data = raw_plot, aes(x = time_ts, y = eda_ts), linetype = "dashed")

    # NOTE: filtered data values are matching matlab filtered data
    # leda$filtered$time_ts[100:110]
    # round(leda$filtered$eda_ts[100:110], 4)


  # 2.2. smoothing
  leda$smoothed <- leda.smoothing(leda$updated$time_ts, leda$updated$eda_ts,
                                                   leda$Hz, leda$opts$smooth$type, leda$opts$smooth$width)
  leda$updated$time_ts <- leda$smoothed$time_ts
  leda$updated$eda_ts <- leda$smoothed$eda_ts
  # Replaced signal with gsignal to have the first eda_ts more similar to matlab, this did not help with reduced length of leda$smoothed$eda_ts
  # Plotting smoothed signal
  smoothed_plot <- data.frame(time_ts = leda$updated$time_ts, eda_ts = leda$updated$eda_ts)
  raw_plot <- data.frame(time_ts = leda$raw$time_ts, eda_ts = leda$raw$eda_ts)
  ggplot() +
    geom_line(data = smoothed_plot, aes(x = time_ts, y = eda_ts), color = "blue") +
    geom_line(data = raw_plot, aes(x = time_ts, y = eda_ts), linetype = "dashed") +
    xlim(100, 270) + ylim(17,18.5)

  # testing smoothing
    # NOTE: smoothing data values are matching matlab smoothing data just that matlab data is 10 timestamps ahead of R
    # leda$smoothed$time_ts[100:110]
    # round(leda$smoothed$eda_ts[100:110], 4)


  # 2.3 Artifact management

  # artifact detection
  leda$artifacts <- leda.artifact_detection(leda$updated$time_ts, leda$updated$eda_ts, leda$Hz, leda$opts$artifact)



  # artifact correction



  # -------- 3. analysis ---------

  # 3.1. CDA analysis
  # nr_iv = number of initial values for optimization
  leda_cda_0 <- leda.sdeco(leda) # right now running without deconv_apply or optimize for debugging
  plot(leda_cda_0$analysis0$tonicDriver, type = "l")

  # phasicData, tonicData, kernel also same. Driver slightly (0.1) lower range in R than matlab. tonicDriver different in 2nd decimal place.

  # DEBUGGING:
    # - sdeconv_analysis and deconv_optimize converging almost (different in 2nd decimal digit) on correct set of tau values
    # - next need to check deconv_apply

  leda_cda_apply <- deconv_apply(leda_cda_0)

  # 3.2. export ERA (event related analysis)
  leda_cda_apply_2 <- trough2peak_analysis(leda_cda_apply)
  leda_export_era <- export_era(leda_cda_apply) # this function changes the number of items in impulsePeakTime

  # 3.3 export SCR list
  leda_export_scr <- export_scrlist(leda_cda_apply_2)




  # -------- 4. plotting and saving ---------

  # 4.1 plot decomposed data
  plot_decomposed_data(leda, 1)

  # 4.2 save output data to ep.physio data structure
  ep.physio <- save_to_ep.physio(ep.physio, leda)


  return(list(ep.physio = ep.physio, leda = leda))
}








