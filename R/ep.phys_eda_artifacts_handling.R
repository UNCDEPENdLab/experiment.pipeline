# for testing
phys_config <- list()
phys_config$eda_preproc <- list()
phys_config$eda_preproc$artifact_detection <- list()
phys_config$eda_preproc$artifact_detection$flag_low_avg <- 1
phys_config$eda_preproc$artifact_detection$max_insta_slope <- 10
phys_config$eda_preproc$artifact_detection$min_out_range <- 0.05
phys_config$eda_preproc$artifact_detection$max_out_range <- 40
phys_config$eda_preproc$artifact_detection$rise_drop_threshold <- 2
phys_config$eda_preproc$artifact_detection$rise_drop_window <- 1
phys_config$eda_preproc$artifact_detection$movingavg_max_inc <- 15
phys_config$eda_preproc$artifact_detection$movingavg_max_dec <- 5
phys_config$eda_preproc$artifact_detection$sd_change_time_inc <- 0.2
phys_config$eda_preproc$artifact_detection$sd_change_time_dec <- 0.5
phys_config$eda_preproc$plot$minor_grid_size <- 5
phys_config$eda_preproc$plot$major_grid_size <- 50
load("/proj/mnhallqlab/studies/neuromap/data/physio_s3/kingdom/preproc/downsampled/downsampled_data_DTK_715.RData")
ep.physio <- list()
ep.physio$eda <- list()
ep.physio$eda$updated <- list()
ep.physio$eda$updated$time_s <- acq_down_dtk$raw$time_s
ep.physio$eda$updated$eda <- acq_down_dtk$raw$EDA...EDA..X..PPGED.R
ep.physio <- ep.phys_eda_artifact_detection(ep.physio, phys_config)

#' This script runs artifact detection on downsampled physio data.
#' @description If multiple subjects are processed together, the same pptx file and same xlsx file will be used for all subjects.
#' @param ep.physio ep.physio structure
#' @param phys_config list of physio specific configuration parameters
#' @return ep.physio structure
#' @author Nidhi Desai
#' 
ep.phys_eda_artifact_detection <- function(ep.physio, phys_config){

    df <- data.frame(time = ep.physio$eda$updated$time_s, eda = ep.physio$eda$updated$eda)
    
    # ---- 1. Average EDA value less than 1 uS ----
    if (mean(df$eda) <= phys_config$eda_preproc$artifact_detection$flag_low_avg){
        # print no further analysis will be done for this subject as EDA is less than 1 uS
        print("average EDA is less than 1 uS. No further EDA analysis will be done for subject", toString(ep.physio$metadata$subID))
        return(0)
    }

    # ---- plot data signal ---- 
    x_range <- range(df$time)
    y_range <- range(df$eda)
    x_major_ticks <- seq(floor(x_range[1]/phys_config$eda_preproc$plot$major_grid_size)*phys_config$eda_preproc$plot$major_grid_size, ceiling(x_range[2]/phys_config$eda_preproc$plot$major_grid_size)*phys_config$eda_preproc$plot$major_grid_size, by = phys_config$eda_preproc$plot$major_grid_size)
    x_minor_ticks <- seq(floor(x_range[1]/phys_config$eda_preproc$plot$minor_grid_size)*phys_config$eda_preproc$plot$minor_grid_size, ceiling(x_range[2]/phys_config$eda_preproc$plot$minor_grid_size)*phys_config$eda_preproc$plot$minor_grid_size, by = phys_config$eda_preproc$plot$minor_grid_size)
    p <- ggplot(df, aes(x = time, y = eda)) + geom_line() + scale_x_continuous(breaks = x_major_ticks, minor_breaks = x_minor_ticks) + ylab("uS")
    label_p <- paste("SubID:", toString(ep.physio$metadata$subID))
  
    # --- dataframes to save issue timepoints ----
    large_insta_slope <- data.frame(subID = numeric(), start_timepoint = numeric(), insta_slope = numeric())
    # out_of_range <- data.frame(subID = numeric(), perct_out_range = numeric(), min_EDA = numeric(), max_EDA = numeric())
    out_of_range <- data.frame(start_timepoint = numeric(), end_timepoint = numeric(), mean_eda = numeric())
    rate_of_change <- data.frame(subID = numeric(), window_start_time = numeric(), window_end_time = numeric(), eda_start = numeric(), eda_end = numeric())    
    moving_average <- data.frame(subID = numeric(), signal_start_time = numeric(), signal_end_time = numeric(), prev_eda_avg = numeric(), eda_start = numeric(), eda_end = numeric())
    change_sd <- data.frame(subID = numeric(), window_start_time = numeric(), window_end_time = numeric(), eda_start = numeric(), eda_end = numeric())

    # ---- 2. Maximum instantaneous slope of EDA data in uS/sec (e.g., 10) ----
    # reference: https://github.com/iankleckner/EDAQA/blob/master/EDAQA-2019.02.11/run_automated_EDAQA.m
    data_uS_per_sec <- diff(df$eda) / ep.physio$downsampled$sampling_rate
    indx <- which(abs(data_uS_per_sec) > phys_config$eda_preproc$plot$max_insta_slope)
    if (length(indx) > 0){
        for (t in indx){
            large_insta_slope <- large_insta_slope %>% add_row(subID = ep.physio$metadata$subID, insta_slope_timepoint = df$time[t], insta_slope = data_uS_per_sec[t])
            p  <- p + geom_point(data = df[t, ], aes(x = time, y = eda), color = "black", shape = 24, size = 3)
        }
    }

    # ---- 3. EDA out of range (uS) ----
    if (min(df$eda) < phys_config$eda_preproc$artifact_detection$min_out_range | max(df$eda) >phys_config$eda_preproc$artifact_detection$max_out_range) {
        # perct_out_range <- round((length(which(df$eda <min_out_range | df$eda >max_out_range))/nrow(df))*100, 2) # percent of time points which have eda value out of range
        
        indx <- which(df$eda < phys_config$eda_preproc$artifact_detection$min_out_range | df$eda > phys_config$eda_preproc$artifact_detection$max_out_range)
        breaks <- c(0, which(diff(indx) != 1), length(indx)) # Identify breaks in the sequence of indices
        for (i in 1:(length(breaks) - 1)) { # Loop through breaks to determine the start and end points of continuous segments
            out_of_range <- out_of_range %>% add_row(start_timepoint = df$time[indx[breaks[i] + 1]], end_timepoint = df$time[indx[breaks[i + 1]]], 
                                                        mean_eda = mean(df$eda[indx[breaks[i] + 1]:breaks[i + 1]]))
            p <- p + geom_line(data = df[indx,], aes(x = time, y = eda), color = "blue")
        }
        out_of_range$subID <- ep.physio$metadata$subID
    }

    # ---- 4. EDA rises/drops quickly ----
    # EDA rises/drops more than rise_drop_threshold uS in less than rise_drop_window seconds
    # Reference: https://github.com/shkurtagashi/EDArtifact/blob/master/EDArtifact_Dashboard/Artifacts_Labelling_Instructions.pdf
    window_start_time <- df$time[1]
    while((window_start_time + phys_config$eda_preproc$artifact_detection$rise_drop_window) <= (max(df$time))){
        window_end_time <- window_start_time +  phys_config$eda_preproc$artifact_detection$rise_drop_window
        indx <- which(df$time >= window_start_time & df$time <= window_end_time)
        if ((max(df$eda[indx]) - min(df$eda[indx])) > phys_config$eda_preproc$artifact_detection$rise_drop_threshold){
            if (nrow(rate_of_change) == 0){
                rate_of_change <- rate_of_change %>% add_row(subID = ep.physio$metadata$subID, window_start_time = window_start_time, window_end_time = window_end_time, 
                                                                            eda_start = min(df$eda[indx[1]]), eda_end = max(df$eda[indx[length(indx)]]))
            } else if(window_start_time == rate_of_change[nrow(rate_of_change), "window_end_time"]){ # add to the same row
                rate_of_change[nrow(rate_of_change), "window_end_time"] <- window_end_time
            } else {
                rate_of_change <- rate_of_change %>% add_row(subID = ep.physio$metadata$subID, window_start_time = window_start_time, window_end_time = window_end_time,
                                                                            eda_start = min(df$eda[indx[1]]), eda_end = max(df$eda[indx[length(indx)]]))
            }
        }
        window_start_time <- window_start_time + phys_config$eda_preproc$artifact_detection$rise_drop_window
    }
    # find the range of time to highlight in the plot
    if (nrow(rate_of_change) > 0){
        highlighted_data <- data.frame()
        for (i in 1:nrow(rate_of_change)) {
            segment <- df %>% filter(time >= rate_of_change$window_start_time[i] & time <= rate_of_change$window_end_time[i]) %>% mutate(group = i)
            highlighted_data <- bind_rows(highlighted_data, segment)
        }
        p <- p + geom_line(data = highlighted_data, aes(x = time, y = eda, group = group), color = "red")
        label_p <- paste0(label_p, "; percent signal EDA rises/drops quickly: ", toString(round((nrow(highlighted_data)/nrow(df))*100, 2)), "%")                   
    }

    # ---- 5. increase in EDA signal of more than X% in 1s or decrease of more than Y% in 1s from previous moving average ----
    moving_avg_window_samples <- acq_down_dtk$sampling_rate # 1 second * sampling rate
    for(i in (moving_avg_window_samples+1):nrow(df)){
        window_start_indx <- i - moving_avg_window_samples
        window_end_indx <- i-1
        mov_avg <- mean(df$eda[window_start_indx:window_end_indx])
        new_eda <- df$eda[i]
        new_time_point <- df$time[i]
        if (mov_avg > 0){
            # eda signal increases by more than 20% in 1s or decreases by more than 10% in 1s from previous moving average
            if (((100*(new_eda - mov_avg)/mov_avg) > phys_config$eda_preproc$artifact_detection$movingavg_max_inc) | ((100*(mov_avg - new_eda)/mov_avg) > phys_config$eda_preproc$artifact_detection$movingavg_max_dec)){
                if (nrow(moving_average) == 0){
                    moving_average <- moving_average %>% add_row(subID = ep.physio$metadata$subID, signal_start_time = new_time_point, signal_end_time = new_time_point, 
                                                                                prev_eda_avg = mov_avg, eda_start = new_eda, eda_end = new_eda)
                } else if (moving_average[nrow(moving_average), "signal_end_time"] == df$time[window_end_indx]){ # update the previous row
                        moving_average[nrow(moving_average), "signal_end_time"] <- new_time_point
                        moving_average[nrow(moving_average), "eda_end"] <- new_eda
                } else { # add a new row
                    moving_average <- moving_average %>% add_row(subID = ep.physio$metadata$subID, signal_start_time = new_time_point, signal_end_time = new_time_point, 
                                                                                prev_eda_avg = mov_avg, eda_start = new_eda, eda_end = new_eda)
                }
            }
        }
    }
    # find the range of time to highlight in the plot
    if (nrow(moving_average) > 0){
        highlighted_data <- data.frame()
        for (i in 1:nrow(moving_average)) {
            segment <- df %>% filter(time >= moving_average$signal_start_time[i] & time <= moving_average$signal_end_time[i]) %>%  mutate(group = i)
            highlighted_data <- bind_rows(highlighted_data, segment)
        }
        p <- p + geom_line(data = highlighted_data, aes(x = time, y = eda, group = group), color = "yellow")               
    }

    # ---- 6. change in EDA signal of more than 1 standard deviation in X seconds ----
    # Reference: MNH provided this rule
    sd_rate_inc <- sd(df$eda)/phys_config$eda_preproc$artifact_detection$sd_change_time_inc # EDA standard deviations per second
    sd_rate_dec <- sd(df$eda)/phys_config$eda_preproc$artifact_detection$sd_change_time_dec # EDA standard deviations per second
    for(i in 1:(nrow(df)-1)){
        if ((df$eda[i+1] - df$eda[i]) > 0){
            if (abs(df$eda[i+1] - df$eda[i]) > (sd_rate_inc*(df$time[i+1]-df$time[i]))){
                if (nrow(change_sd) == 0){
                    change_sd <- change_sd %>% add_row(subID = ep.physio$metadata$subID, window_start_time = df$time[i], window_end_time = df$time[i+1], 
                                                                                eda_start = df$eda[i], eda_end = df$eda[i+1])
                } else {
                    if(df$time[i] == change_sd[nrow(change_sd), "window_end_time"]){ # add to the same row
                        change_sd[nrow(change_sd), "window_end_time"] <- df$time[i+1]
                        change_sd[nrow(change_sd), "eda_end"] <- df$eda[i+1]
                    } else {
                        change_sd <- change_sd %>% add_row(subID = ep.physio$metadata$subID, window_start_time = df$time[i], window_end_time = df$time[i+1],
                                                                                    eda_start = df$eda[i], eda_end = df$eda[i+1])
                    }
                }
            }
        } else if ((df$eda[i+1] - df$eda[i]) < 0){
            if (abs(df$eda[i+1] - df$eda[i]) > (sd_rate_dec*(df$time[i+1]-df$time[i]))){
                if (nrow(change_sd) == 0){
                    change_sd <- change_sd %>% add_row(subID = ep.physio$metadata$subID, window_start_time = df$time[i], window_end_time = df$time[i+1], 
                                                                                eda_start = df$eda[i], eda_end = df$eda[i+1])
                } else {
                    if(df$time[i] == change_sd[nrow(change_sd), "window_end_time"]){ # add to the same row
                        change_sd[nrow(change_sd), "window_end_time"] <- df$time[i+1]
                        change_sd[nrow(change_sd), "eda_end"] <- df$eda[i+1]
                    } else {
                        change_sd <- change_sd %>% add_row(subID = ep.physio$metadata$subID, window_start_time = df$time[i], window_end_time = df$time[i+1],
                                                                                    eda_start = df$eda[i], eda_end = df$eda[i+1])
                    }
                }
            }
        }
    }
    # find the range of time to highlight in the plot
    if (nrow(change_sd) > 0){
        highlighted_data <- data.frame()
        for (i in 1:nrow(change_sd)) {
            segment <- df %>% filter(time >= change_sd$window_start_time[i] & time <= change_sd$window_end_time[i]) %>%  mutate(group = i)
            highlighted_data <- bind_rows(highlighted_data, segment)
        }
        p <- p + geom_line(data = highlighted_data, aes(x = time, y = eda, group = group), color = "green")          
    }

    # ---- Plot the signal in an interactive session using plotly ----
    interactive_plot  <- ggplotly(p, dynamicTicks = TRUE) %>% rangeslider() %>%  layout(hovermode = "x", xaxis = list(fixedrange = FALSE), yaxis = list(fixedrange = FALSE) )
    htmlwidgets::saveWidget(interactive_plot, "interactive_plot.html") # save in a html file if using remove vs code and interactive plot does not open

    # ---- Open a DataEditR browser to update manual checks ----
    # After editing the table, press synchronize and done to update the data frame
    if(nrow(large_insta_slope) == 0 & nrow(out_of_range) == 0 & nrow(rate_of_change) == 0 & nrow(moving_average) == 0 & nrow(change_sd) == 0) {
        print(paste("No artifacts detected for subject", toString(ep.physio$metadata$subID), "."))
    } else {
        print("Multiple artifacts detected. Please check the artifacts in the interactive plot and update the timestamps in the artifacts excel file.") 
        artifacts_table <- data.frame(color = character(), type = character() , start_timepoint = numeric(), end_timepoint = numeric(), prev_eda = numeric(), end_eda = numeric())
        if (nrow(out_of_range) > 0) {
            artifacts_table <- bind_rows(artifacts_table, out_of_range %>% rename(end_eda = mean_eda) %>% mutate(prev_eda = NA) %>% 
                    mutate(color = "blue") %>% mutate(type = "out of range") %>% select(color, type, start_timepoint, end_timepoint, prev_eda, end_eda))
        }
        if(nrow(rate_of_change) > 0){ 
            artifacts_table <- bind_rows(artifacts_table, rate_of_change %>% rename(start_timepoint = window_start_time) %>% rename(end_timepoint = window_end_time) %>% 
                                rename(prev_eda = eda_start) %>% rename(new_eda = eda_end) %>% mutate(color = "red") %>% mutate(type = "quick rise/drop")) %>%
                                select(color, type, start_timepoint, end_timepoint, prev_eda, new_eda)
        }
        if(nrow(moving_average) > 0){ 
            artifacts_table <- bind_rows(artifacts_table, moving_average %>% rename(start_timepoint = signal_start_time) %>% rename(end_timepoint = signal_end_time) %>% 
                                rename(prev_eda = prev_eda_avg) %>% rename(new_eda = eda_end) %>% mutate(color = "yellow") %>% mutate(type = "quick increase/decrease from previous moving average")) %>%
                                select(color, type, start_timepoint, end_timepoint, prev_eda, new_eda)
        }
        if(nrow(change_sd) > 0){
            artifacts_table <- bind_rows(artifacts_table, change_sd %>% rename(start_timepoint = window_start_time) %>% rename(end_timepoint = window_end_time) %>% 
                                rename(prev_eda = eda_start) %>% rename(new_eda = eda_end) %>% mutate(color = "green") %>% mutate(type = "1 standard deviation change")) %>%
                                select(color, type, start_timepoint, end_timepoint, prev_eda, new_eda)
        }
        # interactively edit the artifacts
        artifacts_table <- DataEditR::data_edit(artifacts_table)
        
        # Save the artifacts in an ep.physio structure    
        ep.physio$eda$artifacts <- artifacts_table
    }

    return(ep.physio)
}


#' This script runs artifact correction after artifact detection on downsampled physio data.
#' @param ep.physio ep.physio structure
#' @param phys_config list of physio specific configuration parameters
#' @return ep.physio structure
#' @author Nidhi Desai
#' 
ep.phys_eda_artifact_correction <- function(ep.physio, phys_config){
    
    # Extract timepoints which need to be replaced
    df <- ep.physio$eda$artifacts %>% select(start_timepoint, end_timepoint) %>% rename(start = start_timepoint, stop = end_timepoint)
    replace_artifacts <- merge_intervals(df) # merge_intervals function names the columns as start and stop

    # 1. if the artifact goes down and comes up to similar levels in a short period of time, then replace the timepoints with a spline interpolation
    ep.physio$eda$artifact_corrected <- data.frame(time_s = ep.physio$eda$updated$time_s, eda = replace_artifacts_with_spline(ep.physio$eda$updated, replace_artifacts, u_shape_artifact_threshold = 0.05, max_time_length = 5))
    
    # 2. if the artifact goes for a long time, then replace the timepoint's EDA with NA, so that no further analysis is done on those timepoints
    ep.physio$eda$artifact_corrected <- data.frame(time_s = ep.physio$eda$artifact_corrected$time_s, eda = replace_long_artifacts_with_NA(ep.physio$eda$artifact_corrected, replace_artifacts, max_time_length = 5))

    # plot original and corrected signal
    p <- ggplot(ep.physio$eda$updated, aes(x = time_s, y = eda)) + geom_line() + geom_line(data = ep.physio$eda$artifact_corrected, aes(x = time_s, y = eda), color = "red")
    interactive_plot  <- ggplotly(p, dynamicTicks = TRUE) %>% rangeslider() %>%  layout(hovermode = "x", xaxis = list(fixedrange = FALSE), yaxis = list(fixedrange = FALSE) )
    htmlwidgets::saveWidget(interactive_plot, "interactive_plot_corrected.html") # save in a html file if using remove vs code and interactive plot does not open

}

#' Function to merge overlapping intervals. 
#' NOTE: artifacts dataframe columns should have the names start and stop for the beginning and end of each artifact's timepoints.
#' @param intervals dataframe with start and stop columns
#'
#' @return merged_intervals dataframe with merged intervals
#' @author Nidhi Desai
#' 
merge_intervals <- function(intervals) {
  if (nrow(intervals) == 0) return(intervals)
  
  intervals <- intervals[order(intervals$start), ]
  merged_intervals <- data.frame(start = numeric(0), stop = numeric(0))
  
  current_start <- intervals$start[1]
  current_stop <- intervals$stop[1]
  
  for (i in 2:nrow(intervals)) {
    if (intervals$start[i] <= current_stop) {
      current_stop <- max(current_stop, intervals$stop[i])
    } else {
      merged_intervals <- rbind(merged_intervals, data.frame(start = current_start, stop = current_stop))
      current_start <- intervals$start[i]
      current_stop <- intervals$stop[i]
    }
  }
  
  merged_intervals <- rbind(merged_intervals, data.frame(start = current_start, stop = current_stop))
  return(merged_intervals)
}

#' Function to replace artifacts with spline interpolation.
#' NOTE: artifacts dataframe columns should have the names start and stop for the beginning and end of each artifact's timepoints
#' @param eda_data dataframe with time and eda columns
#' @param artifacts dataframe with start and stop columns
#' @param u_shape_artifact_threshold threshold for similar levels
#' @param max_time_length maximum time length for artifacts to be interpolated
#'
#' @return eda_data with artifacts replaced with spline interpolation
#' @author Nidhi Desai
#'
replace_artifacts_with_spline <- function(eda_data, artifacts, u_shape_artifact_threshold = 0.05, max_time_length = 5) {
  for (i in 1:nrow(artifacts)) {
    artifact_start <- artifacts$start[i]
    artifact_stop <- artifacts$stop[i]
    
    # Find indices of the artifact period
    artifact_indices <- which(eda_data$time >= artifact_start & eda_data$time <= artifact_stop)
    
    # Check if EDA levels drop and return to similar levels
    if (length(artifact_indices) > 2) {
        start_value <- eda_data$eda[min(artifact_indices)]
        end_value <- eda_data$eda[max(artifact_indices)]
        
        if ((abs(start_value - end_value) < u_shape_artifact_threshold) & ((artifact_stop - artifact_start) < max_time_length)) { # threshold for similar levels and artifacts are not too long
            # Perform spline interpolation
            before_indices <- which(eda_data$time < artifact_start)
            after_indices <- which(eda_data$time > artifact_stop)
        
            if (length(before_indices) > 0 && length(after_indices) > 0) {
                
                interp_indices <- c(before_indices[length(before_indices)], after_indices[1])
                interp_values <- eda_data$eda[interp_indices]
                interp_times <- eda_data$time[interp_indices]
                spline_fit <- spline(interp_times, interp_values, xout = eda_data$time[artifact_indices])
                # Replace artifact with interpolated values
                eda_data$eda[artifact_indices] <- spline_fit$y
            }
        }
    }
  }
  return(eda_data$eda)
}

# function to replace too long artifacts with NAs
#' NOTE: artifacts dataframe columns should have the names start and stop for the beginning and end of each artifact's timepoints
#' @param eda_data dataframe with time and eda columns
#' @param artifacts dataframe with start and stop columns
#' @param max_time_length maximum time length for artifacts to be interpolated
#' 
#' @return eda_data with artifacts replaced with NA
#' @author Nidhi Desai
#' 
replace_long_artifacts_with_NA <- function(eda_data, artifacts, max_time_length = 5) {
  for (i in 1:nrow(artifacts)) {
    artifact_start <- artifacts$start[i]
    artifact_stop <- artifacts$stop[i]
    
    # Find indices of the artifact period
    artifact_indices <- which(eda_data$time >= artifact_start & eda_data$time <= artifact_stop)
    
    if ((artifact_stop - artifact_start) > max_time_length) {
      eda_data$eda[artifact_indices] <- NA
    }
  }
  return(eda_data$eda)
}
