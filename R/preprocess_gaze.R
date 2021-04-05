
preprocess_gaze <- function(eye, config, header = "4. Gaze preprocessing:"){

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


  ### N.B. 3/8/21 Interpolating eye data, even in a simplistic manner, is a bad idea given that gaze data is very non-smooth. Better to just rely on the good readings and remove trials with too much data missing, rather than trying to save missing samples.
  ### If the user disagrees the function is still available to use on the preprocessed data, but this should not be included in the standard pipeline.
  # ### 4.4 Interpolate
  # if (!"interpolate" %in% names(c.gaze)) {
  #   cat("- 4.4 Interpolate gaze data: SKIP")
  # } else{
  #   dt <- "- 4.4 Interpolate gaze data:"
  #   tryCatch.ep({
  #     eye <- interpolate_eye(eye, config, signal = "gaze")
  #   },describe_text = dt)
  # }

  ### 4.4 Downsample gaze
  if (!"downsample" %in% names(c.gaze)) {
    cat("- 4.4 Downsample gaze: No gaze downsampling configurations supplied. Defaulting to downsampling factor of 50, and method 'mean'")
    eye <- downsample_eye(eye, analog_channels = c("xp", "yp"))
  } else{
    eye$gaze$downsample <- downsample_eye(eye$raw,
                                          downsample_factor = c.gaze[["downsample"]][["factor"]],
                                          analog_channels = c("xp", "yp"),
                                          method = c.gaze[["downsample"]][["method"]])
    cat("- 4.4 Downsample gaze: COMPLETE")
  }

  ### 4.5 Remove impossible values (outside of screen dim)
  cat("- 4.5 Remove impossible values")
  eye$gaze$downsample <- eye$gaze$downsample %>% mutate(xp = ifelse(xp >= eye$metadata$screen.x | xp <= 0, NA, xp),
                                                        yp = ifelse(yp >= eye$metadata$screen.y | yp <= 0, NA, yp),
                                                        xp = ifelse(is.na(yp), NA, xp),
                                                        yp = ifelse(is.na(xp), NA, yp),
  )

  eye$raw <- eye$raw %>% mutate(xp = ifelse(xp >= eye$metadata$screen.x | xp <= 0, NA, xp),
                                                        yp = ifelse(yp >= eye$metadata$screen.y | yp <= 0, NA, yp),
                                                        xp = ifelse(is.na(yp), NA, xp),
                                                        yp = ifelse(is.na(xp), NA, yp),
  )

  return(eye)
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

