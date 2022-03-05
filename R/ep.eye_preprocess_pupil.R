ep.eye_preprocess_pupil <- function(ep.eye,
                                    blink_corr,
                                    filter,
                                    interpolate,
                                    baseline_correction,
                                    downsample,
                                    header = NULL){

  log_chunk_header(header)

  ### 5.1 Extend blinks
  tryCatch.ep({
    ep.eye <- ep.eye_extend_blinks(ep.eye,
                                   ms_before = blink_corr$ms_before,
                                   ms_after = blink_corr$ms_after)
    # stopifnot(all(c("downsample_bins") %in% names(c.ts))) # if omitted, run defaults.
  }, describe_text = "- 5.1 Extend blinks in pupil data:")

  ### 5.2 Filtering
  tryCatch.ep({
    ep.eye <- ep.eye_smooth_pupil(ep.eye,
                                  method = filter$method,
                                  window_length = filter$window_length)
  }, describe_text = "- 5.2 Smoothing pupil data:")

  ### 5.3 Interpolate
  tryCatch.ep({
    ep.eye <- ep.eye_interp_pupil(ep.eye,
                                  algor = interpolate$algor,
                                  maxgap = interpolate$maxgap)
  }, describe_text = "- 5.3 Interpolating pupil data:")

  ### 5.4 Baseline Correction
  tryCatch.ep({
    ep.eye <- ep.eye_baseline_correct(ep.eye,
                               method = baseline_correction$method,
                               dur_ms = baseline_correction$dur_ms,
                               center_on = baseline_correction$center_on)
  }, describe_text = "- 5.4 Baseline correction:")

  ### 5.5 Downsample pupil
  if(!is.null(downsample)){
    tryCatch.ep({
      ep.eye$pupil$downsample <- ep.eye_downsample(ep.eye$pupil$preprocessed,
                                                   sample.rate = ep.eye$metadata$sample.rate,
                                                   downsampled_freq = downsample$downsampled_freq,
                                                   analog_channels = c("ps", "ps_blinkex", "ps_smooth", "ps_interp", "ps_bc", "time_bc"),
                                                   method = downsample$method)
    }, describe_text = "- 5.5 Downsample pupil:")
  }

  return(ep.eye)
}
