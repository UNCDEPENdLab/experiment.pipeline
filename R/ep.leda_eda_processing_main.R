
# ledalab functions should not assume ep.physio data structure.
# The inputs to this function will be an eda signal, time_signal, required input variable values from config file but not the complete config
pacman::p_load(dplyr, gsignal, ggplot2, pracma, yaml, checkmate)
# load("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.physio_dev/ND_local/test_data/acq_data_splice.RData")
load("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/ND_local/acq_data_downsampled_VM_402.RData") # downsampled data
ep.physio <- acq_down

# saving acq data as txt file
txt_filename <- paste0("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/ND_local/downsampled_VM_402.txt")
txt_df <- ep.physio$raw %>% select(time_s, EDA...EDA..X..PPGED.R, ttl_onset) %>% rename(eda = EDA...EDA..X..PPGED.R)
write.table(txt_df, file = txt_filename, sep = "\t", row.names = FALSE, col.names = FALSE)

config_path <- "C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.physio_dev/inst/extdata/ep_configs/neigborhood/neighborhood_phys.yaml"
source("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/R/validate_exp_yaml.R")
config <- validate_exp_yaml(config_path)
phys_config <- config$definitions$phys

setwd("C:/Users/Nidhi/OneDrive - University of North Carolina at Chapel Hill/Documents/GitHub/experiment.pipeline/exp.ledalab_dev/R")
source("ep.leda_initialization.R")
source("ep.leda_preprocessing.R")
source("ep.leda_utils.R")


#' @title 
#' @description
#' 
#' @param ep.physio 
#' @param phys_config
#' 
#' @author Nidhi Desai
#' 
ledalab_eda_main <- function(ep.physio, phys_config){
  
  round(leda$raw$eda_ts[100:110], 4) # Raw eda_ts are matching with ledalab
  
  # 1. initialization
  leda <- leda.initialization(ep.physio, phys_config)
  
  # 2. preprocessing
  
  # 2.1. low-pass Butterworth filter 
  # higher filter order causes smoother filtering.
  # lower minFreq leads to more aggressive filtering meaning minFreq 
  # means that freq higher than this will be filtered out.
  leda$opts$filter <- list(filter_order = 1, minFreq = 5)
  leda$filtered <- leda.filter(leda$raw$time_ts, leda$raw$eda_ts, leda$Hz, leda$opts$filter)

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
  leda$smoothed <- leda.smoothing(leda$filtered$time_ts, leda$filtered$eda_ts, 
                                                   leda$Hz, leda$opts$smooth$type, leda$opts$smooth$width)
  # testing smoothing
    # NOTE: smoothing data values are matching matlab smoothing data just that matlab data is 10 timestamps ahead of R
    # leda$smoothed$time_ts[100:110]
    # round(leda$smoothed$eda_ts[100:110], 4)
  
    
  # 3. analysis
  
  # 3.1. CDA analysis 
  # nr_iv = number of initial values for optimization
  leda.sdeco(leda)
  
  leda$analysis$tau <- 
  leda$analysis$error <- 
  
  # 3.2. event related analysis
  
  # Export ERA
  
  
  # 3.3 scr list
  
  # export scr list
  
  
  # 4. Export data and plots
  
  # 4.1. anaylsis overview
  
  # 4.2 plotting
  
  
  return(leda)
}