#' Display Steps for Eye-Tracking Data Processing
#'
#' This function displays the various steps involved in processing eye-tracking data.
#' Each step includes a keyword, the function invoked, example arguments for the function, and a description of the step.
#'
#' @return None. This function prints the steps to the console.
#' @examples
#' ep.eye_display_steps()
#'
#' @section Steps:
#' \describe{
#'   \item{1. Keyword: config}{
#'     \describe{
#'       \item{Function Invoked:}{\code{ep.eye_setup_proc_config}}
#'       \item{Function Arguments:}{\code{"edf_raw, file_path, header"}}
#'       \item{Description:}{Processes user-supplied configuration yaml}
#'     }
#'   }
#'   \item{2. Keyword: init}{
#'     \describe{
#'       \item{Function Invoked:}{\code{ep.eye_initialize}}
#'       \item{Function Arguments:}{\code{"config_path"}}
#'       \item{Description:}{Initializes the eye-tracking data using configurations from a YAML file.}
#'     }
#'   }
#'   \item{3. Keyword: parse}{
#'     \describe{
#'       \item{Function Invoked:}{\code{ep.eye_setup_proc_config}}
#'       \item{Function Arguments:}{\code{"event_functions_path"}}
#'       \item{Description:}{Parses task-specific events from the eye-tracking data.}
#'     }
#'   }
#'   \item{4. Keyword: gaze_preproc}{
#'     \describe{
#'       \item{Function Invoked:}{\code{ep.eye_setup_proc_config}}
#'       \item{Function Arguments:}{\code{"aoi, downsample"}}
#'       \item{Description:}{Preprocesses gaze data including filtering, downsampling, etc.}
#'     }
#'   }
#'   \item{5. Keyword: pupil_preproc}{
#'     \describe{
#'       \item{Function Invoked:}{\code{ep.eye_setup_proc_config}}
#'       \item{Function Arguments:}{\code{"blink_corr, filter, interpolate, baseline_correction"}}
#'       \item{Description:}{Preprocesses pupil data including blink correction, filtering, etc.}
#'     }
#'   }
#'   \item{6. Keyword: cleanup}{
#'     \describe{
#'       \item{Function Invoked:}{\code{ep.eye_setup_proc_config}}
#'       \item{Function Arguments:}{\code{"globals"}}
#'       \item{Description:}{Cleans up processed data and prepares it for export.}
#'     }
#'   }
#' }
ep.eye_display_steps <- function() {
  steps <- list(
    list(keyword = "config", fname = "ep.eye_setup_proc_config", example_argument = "edf_raw, file_path, header", description = "Processes user-supplied configuration yaml in experiment.pipeline style"),
    list(keyword = "init", fname = "ep.eye_initialize", example_argument = "config_path", description = "Initializes the eye-tracking data using configurations from a YAML file."),
    list(keyword = "parse", fname = "ep.eye_setup_proc_config", example_argument = "event_functions_path", description = "Parses task-specific events from the eye-tracking data."),
    list(keyword = "gaze_preproc", fname = "ep.eye_setup_proc_config", example_argument = "aoi, downsample", description = "Preprocesses gaze data including filtering, downsampling, etc."),
    list(keyword = "pupil_preproc", fname = "ep.eye_setup_proc_config", example_argument = "blink_corr, filter, interpolate, baseline_correction", description = "Preprocesses pupil data including blink correction, filtering, etc."),
    list(keyword = "cleanup", fname = "ep.eye_setup_proc_config", example_argument = "globals", description = "Cleans up processed data and prepares it for export.")
  )

  for (i in seq_along(steps)) {
    step <- steps[[i]]
    cat(paste0(i, ". Keyword: ", step$keyword, "\n",
               "   Function Invoked: ", step$fname, "\n",
               "   Function Arguments: ", step$example_argument, "\n",
               "   Description: ", step$description, "\n\n"))
  }
}
