#' @title Process ecg data through EDA pre-processing
#' @param ep.physio ep.physio structure
#' #@param phys_config list of physio specific configuration parameters
#' 
#' @author Nidhi Desai
#' 
ep.phys_preprocess_eda <- function(ep.physio, phys_config, ...) {
    
    pacman::p_load(ggplot2, dplyr, plotly, editData)
    ######
    ## 1. Artifact detection and correction
    ######
    ep.physio <- ep.phys_eda_artifact_detection(ep.physio, phys_config)
    # For single subject processing, the code will pause here to allow manual checking of artifacts using an interactive plot and interaction dataframe editor.
    # Change the timestamps in the artifacts dataframe, sync and save, if needed.
    # Change the detection algorithm's input parameters, if larger number of artifacts detected needs change.

    ep.physio <- ep.phys_eda_artifact_correction(ep.physio, phys_config)

    ######
    ## ledalab setting initialization (add to ep.phys_initialize later)
    ######
    leda <- leda.initialization(ep.physio, phys_config)

    # TODO need to figure out how to offset time for ledalab
    # issue arising in groundtime variable in sdeco_interimpulsefit function
    time_offset <-  leda$raw$time_ts[1]
    if (time_offset != 0){ leda$raw$time_ts <- leda$raw$time_ts - time_offset }
    if (time_offset != 0){ leda$events_data$event$time <- leda$events_data$event$time - time_offset }


    ######
    ## 2. preprocessing
        # - low-pass Butterworth filter
        # - smoothing
    ######

    # filtering
    leda$filtered <- leda.filter(leda$raw$time_ts, leda$raw$eda_ts, leda$Hz, leda$opts$filter)
    leda$updated$time_ts <- leda$filtered$time_ts
    leda$updated$eda_ts <- leda$filtered$eda_ts

    # smoothing   
    leda$smoothed <- leda.smoothing(leda$updated$time_ts, leda$updated$eda_ts,
                                                   leda$Hz, leda$opts$smooth$type, leda$opts$smooth$width)
    leda$updated$time_ts <- leda$smoothed$time_ts
    leda$updated$eda_ts <- leda$smoothed$eda_ts


    ######
    ## 3. CDA analysis
    ######




}