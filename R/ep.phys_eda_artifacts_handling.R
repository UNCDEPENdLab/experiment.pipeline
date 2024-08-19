############################
##### List of subsidiary functions utilized in `ep.phys_eda_artifacts_handling()`
############################
# - ep.phys_eda_artifact_detection()
# - ep.phys_eda_artifact_correction()
# -- merge_intervals()
# -- replace_artifacts_with_spline()
# -- replace_long_artifacts_with_NA()
############################

# for testing
rm(list = ls())    
pacman::p_load(ggplot2, dplyr, plotly, editData)
phys_config <- list()
phys_config$eda_preproc <- list()
phys_config$eda_preproc$artifact_detection <- list()
phys_config$eda_preproc$artifact_detection$flag_low_avg <- 1
phys_config$eda_preproc$artifact_detection$max_insta_slope <- 10
phys_config$eda_preproc$artifact_detection$min_out_range <- 0.05
phys_config$eda_preproc$artifact_detection$max_out_range <- 40
phys_config$eda_preproc$artifact_detection$rise_drop_threshold <- 2 # in uS
phys_config$eda_preproc$artifact_detection$rise_drop_window <- 0.25 # in seconds
phys_config$eda_preproc$artifact_detection$movingavg_max_inc <- 15
phys_config$eda_preproc$artifact_detection$movingavg_max_dec <- 5
phys_config$eda_preproc$artifact_detection$sd_change_time_inc <- 0.2
phys_config$eda_preproc$artifact_detection$sd_change_time_dec <- 0.5
phys_config$eda_preproc$plot$minor_grid_size <- 5
phys_config$eda_preproc$plot$major_grid_size <- 50
phys_config$eda_preproc$artifact_correction$u_shape_artifact_threshold <- 0.1
phys_config$eda_preproc$artifact_correction$min_timerange_NA <- 5 


#' This script runs artifact detection on downsampled physio data.
#' @description If multiple subjects are processed together, the same pptx file and same xlsx file will be used for all subjects.
#' @param ep.physio ep.physio structure
#' @param phys_config list of physio specific configuration parameters
#' @return ep.physio structure
#' @author Nidhi Desai
#' 
ep.phys_eda_artifact_detection <- function(ep.physio, phys_config, envir = .GlobalEnv){

    running_eda_data <- data.frame(time = ep.physio$eda$updated$time_s, eda = ep.physio$eda$updated$eda)
    
    # ---- 1. Average EDA value less than 1 uS ----
    if ("flag_low_avg" %in% names(phys_config$eda_preproc$artifact_detection)){
      if (mean(running_eda_data$eda) <= phys_config$eda_preproc$artifact_detection$flag_low_avg){
          # print no further analysis will be done for this subject as EDA is less than 1 uS
          print("average EDA is less than 1 uS. No further EDA analysis will be done for subject", toString(ep.physio$metadata$subID))
          return(0)
      }
    }

    # ---- plot data signal ---- 
    x_range <- range(running_eda_data$time)
    y_range <- range(running_eda_data$eda)
    x_major_ticks <- seq(floor(x_range[1]/phys_config$eda_preproc$plot$major_grid_size)*phys_config$eda_preproc$plot$major_grid_size, ceiling(x_range[2]/phys_config$eda_preproc$plot$major_grid_size)*phys_config$eda_preproc$plot$major_grid_size, by = phys_config$eda_preproc$plot$major_grid_size)
    x_minor_ticks <- seq(floor(x_range[1]/phys_config$eda_preproc$plot$minor_grid_size)*phys_config$eda_preproc$plot$minor_grid_size, ceiling(x_range[2]/phys_config$eda_preproc$plot$minor_grid_size)*phys_config$eda_preproc$plot$minor_grid_size, by = phys_config$eda_preproc$plot$minor_grid_size)
    p <- ggplot(running_eda_data, aes(x = time, y = eda)) + geom_line() + scale_x_continuous(breaks = x_major_ticks, minor_breaks = x_minor_ticks) + ylab("uS")
    label_p <- paste("SubID:", toString(ep.physio$metadata$subID))
  
    # --- dataframes to save issue timepoints ----
    large_insta_slope <- data.frame(subID = numeric(), start_time = numeric(), insta_slope = numeric())
    # out_of_range <- data.frame(subID = numeric(), perct_out_range = numeric(), min_EDA = numeric(), max_EDA = numeric())
    out_of_range <- data.frame(start_time = numeric(), end_time = numeric(), mean_eda = numeric())
    rate_of_change <- data.frame(subID = numeric(), window_start_time = numeric(), window_end_time = numeric(), eda_start = numeric(), eda_end = numeric())    
    moving_average <- data.frame(subID = numeric(), signal_start_time = numeric(), signal_end_time = numeric(), prev_eda_avg = numeric(), eda_start = numeric(), eda_end = numeric())
    change_sd <- data.frame(subID = numeric(), window_start_time = numeric(), window_end_time = numeric(), eda_start = numeric(), eda_end = numeric())

    # ---- 2. Maximum instantaneous slope of EDA data in uS/sec (e.g., 10) ----
    # reference: https://github.com/iankleckner/EDAQA/blob/master/EDAQA-2019.02.11/run_automated_EDAQA.m
    if ("max_insta_slope" %in% names(phys_config$eda_preproc$artifact_detection)){
      data_uS_per_sec <- diff(running_eda_data$eda) / ep.physio$downsampled$sampling_rate
      indx <- which(abs(data_uS_per_sec) > phys_config$eda_preproc$artifact_detection$max_insta_slope)
      if (length(indx) > 0){
          for (t in indx){
              large_insta_slope <- large_insta_slope %>% add_row(subID = ep.physio$metadata$subID, insta_slope_timepoint = running_eda_data$time[t], insta_slope = data_uS_per_sec[t])
              p  <- p + geom_point(data = running_eda_data[t, ], aes(x = time, y = eda), color = "black", shape = 24, size = 3)
          }
      }
    }

    # ---- 3. EDA out of range (uS) ----
    if ("min_out_range" %in% names(phys_config$eda_preproc$artifact_detection) & "max_out_range" %in% names(phys_config$eda_preproc$artifact_detection)){
      if (min(running_eda_data$eda) < phys_config$eda_preproc$artifact_detection$min_out_range | max(running_eda_data$eda) >phys_config$eda_preproc$artifact_detection$max_out_range) {
          # perct_out_range <- round((length(which(running_eda_data$eda <min_out_range | running_eda_data$eda >max_out_range))/nrow(running_eda_data))*100, 2) # percent of time points which have eda value out of range
          
          indx <- which(running_eda_data$eda < phys_config$eda_preproc$artifact_detection$min_out_range | running_eda_data$eda > phys_config$eda_preproc$artifact_detection$max_out_range)
          breaks <- c(0, which(diff(indx) != 1), length(indx)) # Identify breaks in the sequence of indices
          for (i in 1:(length(breaks) - 1)) { # Loop through breaks to determine the start and end points of continuous segments
              out_of_range <- out_of_range %>% add_row(start_time = running_eda_data$time[indx[breaks[i] + 1]], end_time = running_eda_data$time[indx[breaks[i + 1]]], 
                                                          mean_eda = mean(running_eda_data$eda[indx[breaks[i] + 1]:breaks[i + 1]]))
              p <- p + geom_line(data = running_eda_data[indx,], aes(x = time, y = eda), color = "blue")
          }
          out_of_range$subID <- ep.physio$metadata$subID
      }
    }

    # ---- 4. EDA rises/drops quickly ----
    # EDA rises/drops more than rise_drop_threshold uS in less than rise_drop_window seconds
    # Reference: https://github.com/shkurtagashi/EDArtifact/blob/master/EDArtifact_Dashboard/Artifacts_Labelling_Instructions.pdf
    if ("rise_drop_window" %in% names(phys_config$eda_preproc$artifact_detection)){
      window_start_time <- running_eda_data$time[1]
      while((window_start_time + phys_config$eda_preproc$artifact_detection$rise_drop_window) <= (max(running_eda_data$time))){
          window_end_time <- window_start_time +  phys_config$eda_preproc$artifact_detection$rise_drop_window
          indx <- which(running_eda_data$time >= window_start_time & running_eda_data$time <= window_end_time)
          if ((max(running_eda_data$eda[indx]) - min(running_eda_data$eda[indx])) > phys_config$eda_preproc$artifact_detection$rise_drop_threshold){
              if (nrow(rate_of_change) == 0){
                  rate_of_change <- rate_of_change %>% add_row(subID = ep.physio$metadata$subID, window_start_time = window_start_time, window_end_time = window_end_time, 
                                                                              eda_start = min(running_eda_data$eda[indx[1]]), eda_end = max(running_eda_data$eda[indx[length(indx)]]))
              } else if(window_start_time == rate_of_change[nrow(rate_of_change), "window_end_time"]){ # add to the same row
                  rate_of_change[nrow(rate_of_change), "window_end_time"] <- window_end_time
              } else {
                  rate_of_change <- rate_of_change %>% add_row(subID = ep.physio$metadata$subID, window_start_time = window_start_time, window_end_time = window_end_time,
                                                                              eda_start = min(running_eda_data$eda[indx[1]]), eda_end = max(running_eda_data$eda[indx[length(indx)]]))
              }
          }
          window_start_time <- window_start_time + phys_config$eda_preproc$artifact_detection$rise_drop_window
      }
      # find the range of time to highlight in the plot
      if (nrow(rate_of_change) > 0){
          highlighted_data <- data.frame()
          for (i in 1:nrow(rate_of_change)) {
              segment <- running_eda_data %>% filter(time >= rate_of_change$window_start_time[i] & time <= rate_of_change$window_end_time[i]) %>% mutate(group = i)
              highlighted_data <- bind_rows(highlighted_data, segment)
          }
          p <- p + geom_line(data = highlighted_data, aes(x = time, y = eda, group = group), color = "red")
          label_p <- paste0(label_p, "; percent signal EDA rises/drops quickly: ", toString(round((nrow(highlighted_data)/nrow(running_eda_data))*100, 2)), "%")                   
      }
    }
    
    # ---- 5. increase in EDA signal of more than X% in 1s or decrease of more than Y% in 1s from previous moving average ----
    if ("movingavg_max_inc" %in% names(phys_config$eda_preproc$artifact_detection) & "movingavg_max_dec" %in% names(phys_config$eda_preproc$artifact_detection)){
      moving_avg_window_samples <- acq_down_dtk$sampling_rate # 1 second * sampling rate
      for(i in (moving_avg_window_samples+1):nrow(running_eda_data)){
          window_start_indx <- i - moving_avg_window_samples
          window_end_indx <- i-1
          mov_avg <- mean(running_eda_data$eda[window_start_indx:window_end_indx])
          new_eda <- running_eda_data$eda[i]
          new_time_point <- running_eda_data$time[i]
          if (mov_avg > 0){
              # eda signal increases by more than 20% in 1s or decreases by more than 10% in 1s from previous moving average
              if (((100*(new_eda - mov_avg)/mov_avg) > phys_config$eda_preproc$artifact_detection$movingavg_max_inc) | ((100*(mov_avg - new_eda)/mov_avg) > phys_config$eda_preproc$artifact_detection$movingavg_max_dec)){
                  if (nrow(moving_average) == 0){
                      moving_average <- moving_average %>% add_row(subID = ep.physio$metadata$subID, signal_start_time = new_time_point, signal_end_time = new_time_point, 
                                                                                  prev_eda_avg = mov_avg, eda_start = new_eda, eda_end = new_eda)
                  } else if (moving_average[nrow(moving_average), "signal_end_time"] == running_eda_data$time[window_end_indx]){ # update the previous row
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
              segment <- running_eda_data %>% filter(time >= moving_average$signal_start_time[i] & time <= moving_average$signal_end_time[i]) %>%  mutate(group = i)
              highlighted_data <- bind_rows(highlighted_data, segment)
          }
          p <- p + geom_line(data = highlighted_data, aes(x = time, y = eda, group = group), color = "yellow")               
      }
    }

    # ---- 6. change in EDA signal of more than 1 standard deviation in X seconds ----
    # Reference: MNH provided this rule
    if ("sd_change_time_inc" %in% names(phys_config$eda_preproc$artifact_detection) & "sd_change_time_dec" %in% names(phys_config$eda_preproc$artifact_detection)){
      sd_rate_inc <- sd(running_eda_data$eda)/phys_config$eda_preproc$artifact_detection$sd_change_time_inc # EDA standard deviations per second
      sd_rate_dec <- sd(running_eda_data$eda)/phys_config$eda_preproc$artifact_detection$sd_change_time_dec # EDA standard deviations per second
      for(i in 1:(nrow(running_eda_data)-1)){
          if ((running_eda_data$eda[i+1] - running_eda_data$eda[i]) > 0){
              if (abs(running_eda_data$eda[i+1] - running_eda_data$eda[i]) > (sd_rate_inc*(running_eda_data$time[i+1]-running_eda_data$time[i]))){
                  if (nrow(change_sd) == 0){
                      change_sd <- change_sd %>% add_row(subID = ep.physio$metadata$subID, window_start_time = running_eda_data$time[i], window_end_time = running_eda_data$time[i+1], 
                                                                                  eda_start = running_eda_data$eda[i], eda_end = running_eda_data$eda[i+1])
                  } else {
                      if(running_eda_data$time[i] == change_sd[nrow(change_sd), "window_end_time"]){ # add to the same row
                          change_sd[nrow(change_sd), "window_end_time"] <- running_eda_data$time[i+1]
                          change_sd[nrow(change_sd), "eda_end"] <- running_eda_data$eda[i+1]
                      } else {
                          change_sd <- change_sd %>% add_row(subID = ep.physio$metadata$subID, window_start_time = running_eda_data$time[i], window_end_time = running_eda_data$time[i+1],
                                                                                      eda_start = running_eda_data$eda[i], eda_end = running_eda_data$eda[i+1])
                      }
                  }
              }
          } else if ((running_eda_data$eda[i+1] - running_eda_data$eda[i]) < 0){
              if (abs(running_eda_data$eda[i+1] - running_eda_data$eda[i]) > (sd_rate_dec*(running_eda_data$time[i+1]-running_eda_data$time[i]))){
                  if (nrow(change_sd) == 0){
                      change_sd <- change_sd %>% add_row(subID = ep.physio$metadata$subID, window_start_time = running_eda_data$time[i], window_end_time = running_eda_data$time[i+1], 
                                                                                  eda_start = running_eda_data$eda[i], eda_end = running_eda_data$eda[i+1])
                  } else {
                      if(running_eda_data$time[i] == change_sd[nrow(change_sd), "window_end_time"]){ # add to the same row
                          change_sd[nrow(change_sd), "window_end_time"] <- running_eda_data$time[i+1]
                          change_sd[nrow(change_sd), "eda_end"] <- running_eda_data$eda[i+1]
                      } else {
                          change_sd <- change_sd %>% add_row(subID = ep.physio$metadata$subID, window_start_time = running_eda_data$time[i], window_end_time = running_eda_data$time[i+1],
                                                                                      eda_start = running_eda_data$eda[i], eda_end = running_eda_data$eda[i+1])
                      }
                  }
              }
          }
      }
      # find the range of time to highlight in the plot
      if (nrow(change_sd) > 0){
          highlighted_data <- data.frame()
          for (i in 1:nrow(change_sd)) {
              segment <- running_eda_data %>% filter(time >= change_sd$window_start_time[i] & time <= change_sd$window_end_time[i]) %>%  mutate(group = i)
              highlighted_data <- bind_rows(highlighted_data, segment)
          }
          p <- p + geom_line(data = highlighted_data, aes(x = time, y = eda, group = group), color = "green")          
      }
    }

    # ---- Combine all artifacts ----
    artifacts_table <- data.frame(color = character(), type = character() , start_time = numeric(), end_time = numeric(), prev_eda = numeric(), end_eda = numeric())
    if (nrow(out_of_range) > 0) {
      artifacts_table <- bind_rows(artifacts_table, out_of_range %>% rename(end_eda = mean_eda) %>% mutate(prev_eda = NA) %>% 
                                     mutate(color = "blue") %>% mutate(type = "out of range") %>% select(color, type, start_time, end_time, prev_eda, end_eda))
    }
    if(nrow(rate_of_change) > 0){ 
      artifacts_table <- bind_rows(artifacts_table, rate_of_change %>% rename(start_time = window_start_time) %>% rename(end_time = window_end_time) %>% 
                                     rename(prev_eda = eda_start) %>% rename(new_eda = eda_end) %>% mutate(color = "red") %>% mutate(type = "quick rise/drop")) %>%
        select(color, type, start_time, end_time, prev_eda, new_eda)
    }
    if(nrow(moving_average) > 0){ 
      artifacts_table <- bind_rows(artifacts_table, moving_average %>% rename(start_time = signal_start_time) %>% rename(end_time = signal_end_time) %>% 
                                     rename(prev_eda = prev_eda_avg) %>% rename(new_eda = eda_end) %>% mutate(color = "yellow") %>% mutate(type = "quick increase/decrease from previous moving average")) %>%
        select(color, type, start_time, end_time, prev_eda, new_eda)
    }
    if(nrow(change_sd) > 0){
      artifacts_table <- bind_rows(artifacts_table, change_sd %>% rename(start_time = window_start_time) %>% rename(end_time = window_end_time) %>% 
                                     rename(prev_eda = eda_start) %>% rename(new_eda = eda_end) %>% mutate(color = "green") %>% mutate(type = "1 standard deviation change")) %>%
        select(color, type, start_time, end_time, prev_eda, new_eda)
    }
    
    # ---- Plot the signal in an interactive session using plotly ----
    interactive_plot <- ggplotly(p, dynamicTicks = TRUE) %>% rangeslider() %>%  layout(hovermode = "x", xaxis = list(fixedrange = FALSE), yaxis = list(fixedrange = FALSE) )
    # print(interactive_plot)
    html_file <- tempfile(pattern = "artifact_detection_", fileext = ".html")
    htmlwidgets::saveWidget(interactive_plot, html_file, selfcontained = TRUE)
    browseURL(html_file)
    
    # ---- Open a DataEditR browser to update manual checks ----
    # After editing the table, click done to update the data frame
    if(nrow(large_insta_slope) == 0 & nrow(out_of_range) == 0 & nrow(rate_of_change) == 0 & nrow(moving_average) == 0 & nrow(change_sd) == 0) {
        print(paste("No artifacts detected for subject", toString(ep.physio$metadata$subID), "."))
    } else {
        print("Multiple artifacts detected. Please check the artifacts in the interactive plot and update the timestamps in the artifacts table.") 
      
        # interactively edit the artifacts
        .GlobalEnv$artifacts_table <- artifacts_table
        artifacts_table <- editData::editData(artifacts_table) # DataEditR::data_edit(artifacts_table) # data_edit still doesn't work after assigning to global environment
        rm(artifacts_table, envir = .GlobalEnv)
        
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
    df <- ep.physio$eda$artifacts %>% select(start_time, end_time) %>% rename(start = start_time, stop = end_time)
    replace_artifacts <- merge_intervals(df) # merge_intervals function names the columns as start and stop

    # 1. if the artifact goes down and comes up to similar levels or above in a short period of time, then replace the timepoints with a spline interpolation
    ep.physio$eda$artifact_corrected <- data.frame(time_s = ep.physio$eda$updated$time_s, eda = replace_artifacts_with_spline(ep.physio$eda$updated, replace_artifacts, 
                                                                                                                              u_shape_artifact_threshold = phys_config$eda_preproc$artifact_correction$u_shape_artifact_threshold, 
                                                                                                                              max_time_length = phys_config$eda_preproc$artifact_correction$min_timerange_NA))
    
    # 2. if the artifact goes for a long time, then replace the timepoint's EDA with NA, so that no further analysis is done on those timepoints
    ep.physio$eda$artifact_corrected <- data.frame(time_s = ep.physio$eda$artifact_corrected$time_s, eda = replace_long_artifacts_with_NA(ep.physio$eda$artifact_corrected, replace_artifacts, 
                                                                                                                                          min_time_length = phys_config$eda_preproc$artifact_correction$min_timerange_NA))

    # plot original and corrected signal
    p <- ggplot(ep.physio$eda$updated, aes(x = time_s, y = eda)) + geom_line() + geom_line(data = ep.physio$eda$artifact_corrected, aes(x = time_s, y = eda), color = "lightblue")
    interactive_plot <- ggplotly(p, dynamicTicks = TRUE) %>% rangeslider() %>%  layout(hovermode = "x", xaxis = list(fixedrange = FALSE), yaxis = list(fixedrange = FALSE) )
    html_file_2 <- tempfile(pattern = "artifact_correction_", fileext = ".html")
    htmlwidgets::saveWidget(interactive_plot, html_file_2, selfcontained = TRUE)
    browseURL(html_file_2)
    
    return(ep.physio)
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
    
    # Check if EDA levels drop and return to similar levels or above
    if (length(artifact_indices) > 2) {
        start_value <- eda_data$eda[min(artifact_indices)]
        end_value <- eda_data$eda[max(artifact_indices)]
        
        if ((start_value - end_value < u_shape_artifact_threshold) & ((artifact_stop - artifact_start) < max_time_length)) { # threshold for similar levels and artifacts are not too long
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
#' @param min_time_length maximum time length for artifacts to be interpolated
#' 
#' @return eda_data with artifacts replaced with NA
#' @author Nidhi Desai
#' 
replace_long_artifacts_with_NA <- function(eda_data, artifacts, min_time_length = 5) {
  for (i in 1:nrow(artifacts)) {
    artifact_start <- artifacts$start[i]
    artifact_stop <- artifacts$stop[i]
    
    # Find indices of the artifact period
    artifact_indices <- which(eda_data$time >= artifact_start & eda_data$time <= artifact_stop)
    
    if ((artifact_stop - artifact_start) > min_time_length) {
      eda_data$eda[artifact_indices] <- NA
    }
  }
  return(eda_data$eda)
}




# running 
load("/proj/mnhallqlab/studies/neuromap/data/physio_s3/kingdom/preproc/downsampled/downsampled_data_DTK_368.RData")
ep.physio <- list()
ep.physio$eda <- list()
ep.physio$eda$updated <- data.frame(time_s = acq_down_dtk$raw$time_s, eda = acq_down_dtk$raw$EDA...EDA..X..PPGED.R)
ep.physio <- ep.phys_eda_artifact_detection(ep.physio, phys_config)
ep.physio <- ep.phys_eda_artifact_correction(ep.physio, phys_config)
save(ep.physio, file = "/proj/mnhallqlab/studies/neuromap/data/physio_s3/kingdom/preproc/eda/eda_artifacts_corrected_DTK_368.RData")



