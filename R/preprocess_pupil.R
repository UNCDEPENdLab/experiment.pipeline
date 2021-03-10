preprocess_gaze <- function(eye, config, header = "5. Additional pupil-specific preprocessing:"){

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

    #### need to tweak for different sampling rates.
    all_t <- seq(0,max(eye$raw$time))
    paddf <- data.table(eventn = NA,
                        time = which(!all_t %in% eye$raw$time) -1,
                        ps = NA,
                        saccn = NA,
                        fixn = NA,
                        blinkn = NA,
                        block = NA,
                        block_trial = NA,
                        event = NA,
                        et.msg = NA,
                        ps_blinkex = NA,
                        ps_smooth = NA
    )
    temp <- rbind(eye$pupil$preprocessed, paddf) %>% arrange(time)

    temp$ps_interp <- na_interpolation(temp$ps_smooth, option = "linear", maxgap = 750)

    eye$pupil$preprocessed <- temp %>% filter(!is.na(eventn)) %>% data.table()

  }, describe_text = "- 5.4 Interpolating pupil data:")

  ### 5.5 Baseline Correction
  tryCatch.ep({




  }, describe_text = "- 5.5 Baseline correction:")


  return(eye)
}
