
preprocess_gaze <- function(eye, config, header = "4. Additional gaze-specific preprocessing:"){

  log_chunk_header(header)

  ### 4.1 Extract gaze configuration options
  tryCatch.ep({
    c.gaze <- tidy_eye_config(config)[["gaze"]]
    # stopifnot(all(c("downsample_bins") %in% names(c.ts))) # if omitted, run defaults.
  }, describe_text = "- 4.1 Extract gaze config options:")

  ### 4.2 Assign AOIs
  if (!"aoi" %in% names(c.gaze)) {
    cat("No AOI configurations supplied. Skipping")
  } else{
    cat("- 4.2 Assign AOIs to gaze data\n")
    eye <- add_aois(eye, config)
  }

  ### 4.3 Collapse timestamps
  dt <- "- 4.3 Collapse timestamps with more than one row:"
  eye <- collapse_time(eye, dt)

  ### 4.4 Downsample gaze
  if (!"downsample" %in% names(c.gaze)) {
    cat("No gaze downsampling configurations supplied. Defaulting to downsampling factor of 50, and method 'mean'")
  } else{
    cat("- 4.4 Downsample gaze:\n")
    eye <- downsample_eye(eye,
                          downsample_factor = c.gaze[["downsample"]][["factor"]],
                          analog_channels = c("xp", "yp"),
                          method = c.gaze[["downsample"]][["method"]])
  }


}





#' collapse timing to one row per time (paste multiple messages in a single et.msg)
#'
#'
collapse_time <- function(eye, dt){
  tryCatch.ep({
    eye$raw <- eye$raw %>% data.frame() %>%
      dplyr::group_by(eventn, time, xp, yp, ps, saccn, fixn, blinkn, block, block_trial, event) %>%
      dplyr::summarise(et.msg = paste(et.msg, collapse = " | "), .groups = "drop") %>% data.table()
    # browser()

    if(unique(table(eye$raw$time)) != 1){
      warning("Some measurements have more than one row")
    }
  },
  describe_text = dt
  )
  return(eye)
}




# ignore below ------------------------------------------------------------





#' Tidy timeseries
#'

tidy_eye_timeseries <- function(eye, config, header = "4. Tidy raw timeseries:"){

  log_chunk_header(header)

  ### 4.1 Extract timeseries configuration options
  tryCatch.ep({
    c.ts <- tidy_eye_config(config)[["ts"]]
    # stopifnot(all(c("downsample_bins") %in% names(c.ts))) # if omitted, run defaults.
  }, describe_text = "- 4.1 Extract TS config options:")

  # ### 4.2 Reset timestamps to 0 at first recording
  # tryCatch.ep({
  #   t_start <- eye$raw$time[1]
  #   eye$metadata$t_start <- t_start
  #
  #   # raw
  #   eye$raw$time <- eye$raw$time - t_start
  #   # saccades
  #   eye$gaze$sacc$stime <- eye$gaze$sacc$stime - t_start
  #   eye$gaze$sacc$etime <- eye$gaze$sacc$etime - t_start
  #   # fixations
  #   eye$gaze$fix$stime <- eye$gaze$fix$stime - t_start
  #   eye$gaze$fix$etime <- eye$gaze$fix$etime - t_start
  #   # blinks
  #   eye$gaze$blink$stime <- eye$gaze$blink$stime - t_start
  #   eye$gaze$blink$etime <- eye$gaze$blink$etime - t_start
  #   #meta-data
  #   eye$metadata$missing_measurements$start <- eye$metadata$missing_measurements$start - t_start
  #   eye$metadata$missing_measurements$end <- eye$metadata$missing_measurements$end - t_start
  #   eye$metadata$btw_tr_msg$time <- eye$metadata$btw_tr_msg$time - t_start
  #
  #
  #
  # }, describe_text = "- 4.2 Shift timestamps to 0 start point:")


  ### 4.3 Downsample eye data
  dt <- "- 4.3 Downsample Eye :"
  if("downsample_factor" %in% names(c.ts)){
    if("downsample_method" %in% names(c.ts)){
      tryCatch.ep({
        eye <- downsample_eye(eye, downsample_factor = c.ts$downsample_factor, method = c.ts$downsample_method)
      },describe_text = dt)
    } else { # fallback on default method ("mean")
      tryCatch.ep({
        eye <- downsample_eye(eye, downsample_factor = c.ts$downsample_factor)
      },describe_text = dt)
    }
  } else{ # fallback on downsampling rate of 50 (at 1000Hz this would downsample to 20Hz, or 1 measurement every .05 sec), and "mean" method
    tryCatch.ep({
      eye <- downsample_eye(eye)
    },describe_text = dt)
  }

  ### 4.4 Interpolation
  dt <- "- 4.3 Downsample Gaze:"
  if("downsample_factor" %in% names(c.ts)){
    if("downsample_method" %in% names(c.ts)){
      tryCatch.ep({
        eye <- downsample_eye(eye, downsample_factor = c.ts$downsample_factor, method = c.ts$downsample_method)
      },describe_text = dt)
    } else { # fallback on default method ("mean")
      tryCatch.ep({
        eye <- downsample_eye(eye, downsample_factor = c.ts$downsample_factor)
      },describe_text = dt)
    }
  } else{ # fallback on downsampling rate of 50 (at 1000Hz this would downsample to 20Hz, or 1 measurement every .05 sec), and "mean" method
    tryCatch.ep({
      eye <- downsample_eye(eye)
    },describe_text = dt)
  }



  return(eye)
}


