############################
##### List of subsidiary functions utilized in `ep.eye_setup_proc_config()`
############################
# - ep.eye_default_options()
# - ep.eye_build_msg_seq()
############################

#' Get Default Eye-Tracking Processing Options
#'
#' Returns a hierarchical list containing the default options for the eye-tracking data processing pipeline.
#'
#' @return A named list with default options for the following fields:
#' \itemize{
#'   \item \strong{global:} General processing options, including:
#'   \itemize{
#'     \item \code{base_dir} (character): The base directory, defaults to the current working directory.
#'     \item \code{prefix} (character): A regular expression pattern for the file prefix, defaults to "\\d{2,3}".
#'     \item \code{save_steps} (logical): Flag to save intermediate processing steps, defaults to \code{TRUE}.
#'     \item \code{preproc_out} (character): Directory for preprocessed output, defaults to "preproc_out" in the current working directory.
#'     \item \code{log} (logical): Flag to enable logging, defaults to \code{FALSE}.
#'     \item \code{remove_raw} (logical): Flag to return raw data, defaults to \code{FALSE}.
#'   }
#'   \item \strong{initialize:} Initialization options, including:
#'   \itemize{
#'     \item \code{expected_edf_fields} (character vector): Expected EDF file fields, defaults to \code{c("raw", "sacc", "fix", "blinks", "msg", "input", "button", "info", "asc_file")}.
#'     \item \code{unify_gaze_events} (list): Gaze event unification options, defaults to:
#'     \itemize{
#'       \item \code{gaze_events} (character vector): Gaze events to unify, defaults to \code{c("sacc", "fix", "blink")}.
#'       \item \code{confirm_correspondence} (logical): Flag to confirm correspondence of gaze events, defaults to \code{FALSE}.
#'     }
#'   }
#'   \item \strong{msg_parse:} Message parsing options, defaults to \code{NULL}.
#'   \item \strong{gaze_preproc:} Gaze preprocessing options, including:
#'   \itemize{
#'     \item \code{aoi} (list): Area of interest (AOI) options, defaults to:
#'     \itemize{
#'       \item \code{indicator} (character): Indicator for AOI, defaults to "!V IAREA RECTANGLE".
#'       \item \code{extraction_method} (character): Method for AOI extraction, defaults to "regex".
#'       \item \code{extraction_coords} (character): Regular expression for extracting coordinates, defaults to "\\d{3,4} \\d{3,4} \\d{3,4} \\d{3,4}".
#'       \item \code{extract_labs} (character): Regular expression for extracting labels, defaults to "[a-z]+$".
#'       \item \code{split_coords} (character): Character used to split coordinates, defaults to a single space.
#'       \item \code{tag_raw} (logical): Flag to tag raw data, defaults to \code{FALSE}.
#'     }
#'     \item \code{downsample} (list): Downsampling options, defaults to:
#'     \itemize{
#'       \item \code{downsampled_freq} (numeric): Frequency to downsample to, defaults to \code{50} Hz.
#'       \item \code{method} (character): Method for downsampling, defaults to "mean".
#'     }
#'   }
#'   \item \strong{pupil_preproc:} Pupil preprocessing options, including:
#'   \itemize{
#'     \item \code{blink_corr} (list): Blink correction options, defaults to:
#'     \itemize{
#'       \item \code{ms_before} (numeric): Time before blink in milliseconds, defaults to \code{150}.
#'       \item \code{ms_after} (numeric): Time after blink in milliseconds, defaults to \code{150}.
#'     }
#'     \item \code{filter} (list): Filtering options, defaults to:
#'     \itemize{
#'       \item \code{method} (character): Filtering method, defaults to "movingavg".
#'       \item \code{window_length} (numeric): Length of the filtering window, defaults to \code{20}.
#'     }
#'     \item \code{interpolate} (list): Interpolation options, defaults to:
#'     \itemize{
#'       \item \code{algor} (character): Interpolation algorithm, defaults to "linear".
#'       \item \code{maxgap} (numeric): Maximum gap for interpolation in milliseconds, defaults to \code{1000}.
#'     }
#'     \item \code{baseline_correction} (list): Baseline correction options, defaults to:
#'     \itemize{
#'       \item \code{method} (character): Method for baseline correction, defaults to "subtract".
#'       \item \code{dur_ms} (numeric): Duration in milliseconds for baseline correction, defaults to \code{100}.
#'       \item \code{center_on} (character): Event to center baseline correction on, defaults to "DISPLAY_ON".
#'     }
#'     \item \code{downsample} (list): Downsampling options, defaults to:
#'     \itemize{
#'       \item \code{downsampled_freq} (numeric): Frequency to downsample to, defaults to \code{20} Hz.
#'       \item \code{method} (character): Method for downsampling, defaults to "mean".
#'     }
#'   }
#' }
#'
#' @examples
#' defaults <- ep.eye_default_options()
#' str(defaults)
#'
#' @author Nate Hall
#' @export
#'
ep.eye_default_options <- function(edf_raw) {
  defaults <- list(
    global = list(
      task = "experiment.pipeline_default",
      base_dir = getwd(),
      id = "\\d{2,3}",
      save_steps = TRUE,
      preproc_out = file.path(getwd(), "preproc_out"),
      log = FALSE,
      remove_raw = FALSE
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

#' @title Build out expected message sequences within config file.
#'
#' @description When a message sequence check is requested, the user specifies event-general start and end message sequences, with the message sent during the event being unique to the block and event. This function attempts to combine the general and specific into the msg_seq field of msg_parse options, which gives block/event-level specificity on the exact expected sequence of messages to check.
#' @param config Named list of configuration options read in by \code{validate_exp_yaml}
#' @param dt Descriptive text to print after running. Defaults to NULL (silent).
#'
#' @return Nested list with configuration options.
#'
#' @author Nate Hall
#'
#' @export
ep.eye_build_msg_seq <- function(config, dt = NULL){
  tryCatch.ep({
    c.e <- config[["definitions"]][["eye"]]
    event_info <- c.e[["msg_parse"]]

    if("msg_seq" %in% names(event_info)){
      if("eval_middle" %in% names(event_info[["msg_seq"]])){
        for(i in names(config[["blocks"]])){
          # check first for an eye field in each event type in a block.
          for(j in names(config[["blocks"]][[i]][["events"]])){
            ev_m <- config[["blocks"]][[i]][["events"]][[j]][["eye"]]
            if(event_info[["msg_seq"]][["eval_middle"]]){
              msg_vec <- c(event_info[["msg_seq"]][["msg_start"]], ev_m[["mid_msg"]], event_info[["msg_seq"]][["msg_end"]])
              c.e[["msg_parse"]][["msg_seq"]][[i]][[j]] <- msg_vec
            }
          }
        }
      }
    }
  }, describe_text = dt)
  return(c.e)
}
