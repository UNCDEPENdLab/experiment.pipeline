preprocess_pupil <- function(eye, config, header = "5. Pupil preprocessing:"){

  log_chunk_header(header)

  ### 5.1 Extract pupil configuration options
  tryCatch.ep({
    c.pupil <- tidy_eye_config(config)[["pupil"]]
    # stopifnot(all(c("downsample_bins") %in% names(c.ts))) # if omitted, run defaults.
  }, describe_text = "- 5.1 Extract pupil config options:")

  ### 5.2 Extend blinks
  tryCatch.ep({
    eye <- extend_blinks(eye, c.pupil)
    # stopifnot(all(c("downsample_bins") %in% names(c.ts))) # if omitted, run defaults.
  }, describe_text = "- 5.2 Extend blinks in pupil data:")

  ### 5.3 Filtering
  tryCatch.ep({
    eye <- smooth_pupil(eye, c.pupil)
    # stopifnot(all(c("downsample_bins") %in% names(c.ts))) # if omitted, run defaults.
  }, describe_text = "- 5.3 Smoothing pupil data:")

  ### 5.4 Interpolate
  tryCatch.ep({
    eye <- interp_pupil(eye, maxgap = c.pupil$interpolate$maxgap)
  }, describe_text = "- 5.4 Interpolating pupil data:")

  ### 5.5 Baseline Correction
  tryCatch.ep({
    eye <- baseline_correct(eye, center_on = c.pupil$baseline_correction$center_on, dur_ms = c.pupil$baseline_correction$dur_ms)
  }, describe_text = "- 5.5 Baseline correction:")

  ## 5.6 Downsample pupil
  if("downsample" %in% names(c.pupil)){
    tryCatch.ep({
      eye$pupil$downsample <- downsample_eye(eye[["pupil"]][["preprocessed"]],
                                             downsample_factor = c.pupil[["downsample"]][["factor"]],
                                             analog_channels = c("ps", "ps_blinkex", "ps_smooth", "ps_interp", "ps_bc", "time_bc"),
                                             method = c.pupil[["downsample"]][["method"]])
    }, describe_text = "- 5.6 Downsample pupil:")

  }

  return(eye)
}
