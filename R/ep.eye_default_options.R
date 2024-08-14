#' Default Options for Eye-Tracking Preprocessing
#'
#' This function returns a list of default options for various stages of eye-tracking data preprocessing (to be combined with user-inputs).
#'
#' @return A list containing default options for global settings, initialization, message parsing, gaze preprocessing, and pupil preprocessing.
#' @export
#' @author Nate Hall
#' @examples
#' defaults <- ep.eye_default_options()
#' print(defaults)

ep.eye_default_options <- function() {
  defaults <- list(
    global = list(
      base_dir = getwd(),
      prefix = "\\d{2,3}",
      save_steps = TRUE,
      preproc_out = file.path(getwd(), "preproc_out"),
      log = FALSE,
      return_raw = FALSE
    ),
    initialize = list(
      expected_edf_fields = c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file"),
      unify_gaze_events = list(
        gaze_events = c("sacc", "fix", "blink"),
        confirm_correspondence = FALSE
      )
    ),
    msg_parse = NULL,  # No default options specified
    gaze_preproc = list(
      aoi = list(
        indicator = "!V IAREA RECTANGLE",
        extraction_method = "regex",
        extraction_coords = "\\d{3,4} \\d{3,4} \\d{3,4} \\d{3,4}",
        extract_labs = "[a-z]+$",
        split_coords = " ",
        tag_raw = FALSE
      ),
      downsample = list(
        downsampled_freq = 50,
        method = "mean"
      )
    ),
    pupil_preproc = list(
      blink_corr = list(
        ms_before = 150,
        ms_after = 150
      ),
      filter = list(
        method = "movingavg",
        window_length = 20
      ),
      interpolate = list(
        algor = "linear",
        maxgap = 1000
      ),
      baseline_correction = list(
        method = "subtract",
        dur_ms = 100,
        center_on = "DISPLAY_ON"
      ),
      downsample = list(
        downsampled_freq = 20,
        method = "mean"
      )
    )
  )
  return(defaults)
}



