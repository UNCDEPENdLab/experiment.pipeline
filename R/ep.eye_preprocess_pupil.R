ep.eye_preprocess_pupil <- function(ep.eye, 
                                    blink_corr,
                                    filter,
                                    interpolate,
                                    baseline_correction,
                                    downsample,
                                    header = NULL){
  ### debug
  ep.eye <- eye_gazePre
  blink_corr = config$definitions$eye$pupil_preproc$blink_corr
  filter = config$definitions$eye$pupil_preproc$filter
  interpolate = config$definitions$eye$pupil_preproc$interpolate
  baseline_correction = config$definitions$eye$pupil_preproc$baseline_correction
  downsample = config$definitions$eye$pupil_preproc$downsample
  header = "5. Pupil preprocessing:"

  log_chunk_header(header)

  ### 5.1 Extend blinks
  tryCatch.ep({
    ep.eye <- ep.eye_extend_blinks(ep.eye,
                                   sample.rate = ep.eye$metadata$sample.rate,   
                                   ms_before = blink_corr$ms_before,
                                   ms_after = blink_corr$ms_after)
    # stopifnot(all(c("downsample_bins") %in% names(c.ts))) # if omitted, run defaults.
  }, describe_text = "- 5.1 Extend blinks in pupil data:")

  ### 5.2 Filtering
  tryCatch.ep({
    ep.eye <- smooth_pupil(ep.eye, c.pupil)
    # stopifnot(all(c("downsample_bins") %in% names(c.ts))) # if omitted, run defaults.
  }, describe_text = "- 5.2 Smoothing pupil data:")

  ### 5.4 Interpolate
  tryCatch.ep({
    ep.eye <- interp_pupil(ep.eye, maxgap = c.pupil$interpolate$maxgap)
  }, describe_text = "- 5.4 Interpolating pupil data:")

  ### 5.5 Baseline Correction
  tryCatch.ep({
    ep.eye <- baseline_correct(ep.eye, center_on = c.pupil$baseline_correction$center_on, dur_ms = c.pupil$baseline_correction$dur_ms)
  }, describe_text = "- 5.5 Baseline correction:")

  ## 5.6 Downsample pupil
  if("downsample" %in% names(c.pupil)){

      ep.eye$pupil$downsample <- downsample_eye(ep.eye[["pupil"]][["preprocessed"]],
                                             downsample_factor = c.pupil[["downsample"]][["factor"]],
                                             analog_channels = c("ps", "ps_blinkex", "ps_smooth", "ps_interp", "ps_bc", "time_bc"),
                                             method = c.pupil[["downsample"]][["method"]])
      cat("- 5.6 Downsample pupil: COMPLETE\n")
  }

  return(ep.eye)
}
