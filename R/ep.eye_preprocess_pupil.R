#' Preprocess Pupil Data
#'
#' This function preprocesses pupil data by extending blinks, filtering, interpolating, performing baseline correction, and downsampling the data.
#'
#' @param ep.eye A data object containing eye-tracking information.
#' @param config config
#' @param blink_corr A list containing parameters for blink correction: `ms_before` and `ms_after` specifying the milliseconds before and after a blink to be corrected.
#' @param filter A list containing parameters for filtering: `method` and `window_length` specifying the method and window length for filtering.
#' @param interpolate A list containing parameters for interpolation: `algor` and `maxgap` specifying the algorithm and maximum gap for interpolation.
#' @param baseline_correction A list containing parameters for baseline correction: `method`, `dur_ms`, and `center_on` specifying the method, duration in milliseconds, and the centering method for baseline correction.
#' @param downsample A list containing parameters for downsampling: `downsampled_freq` and `method` specifying the downsampled frequency and method for downsampling.
#' @param header An optional header parameter. If NULL, no header is used.
#'
#' @return A data object containing the preprocessed pupil data.
#' @export
#'
#' @examples
#' # Assuming `ep.eye`, `blink_corr`, `filter`, `interpolate`, `baseline_correction`, and `downsample` are defined:
#' preprocessed_data <- ep.eye_preprocess_pupil(ep.eye, blink_corr, filter, interpolate, baseline_correction, downsample)
ep.eye_preprocess_pupil <- function(ep.eye,
                                    config,
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

  ### save preproc'ed ep.eye object to correct folder
  if(config$definitions$eye$global$save_steps){
    pupil_dir <- config$definitions$eye$global$preproc_out %>% file.path(., "ep.eye_preproc_pupil")
    if(!dir.exists(pupil_dir)) {dir.create(pupil_dir, recursive = TRUE)}
    subj_path <- file.path(pupil_dir, paste0(config$definitions$eye$global$id, ".rds"))
    tryCatch.ep({
      saveRDS(ep.eye, subj_path)
    },
    describe_text = paste0("- 5.6 Save preprocessed pupil ep.eye [", subj_path,"]:"))
  }

  return(ep.eye)
}
