#' Eye Data Preprocessing and Cleanup
#'
#' This function processes eye tracking data for use in event-related potential (ERP) studies.
#' It generates event-locked timing, removes raw data to minimize object size, tags the data
#' with a class to indicate successful preprocessing, saves the preprocessed data, and manages log files.
#'
#' @param ep.eye An object containing the eye tracking data to be cleaned up.
#' @param globals A list of global parameters defined in the config file.
#' @param header A string used to set the chunk header for the logging. Default is NULL.
#'
#' @return Returns the cleaned up eye tracking data with the class "ep.eye.preproc".
#'
#' @examples
#' \dontrun{
#' cleaned_data <- ep.eye_cleanup(raw_eye_data, config$definitions$eye$global, "6. Cleanup and export ep.eye")
#' }
#' @export
#'
#' @seealso \code{\link{ep.eye_tag_event_time}}, \code{\link{ep.eye_save_preproc}}

ep.eye_cleanup <- function(ep.eye,
                           globals,
                           header = NULL){

  ### debug
  # ep.eye <- eye_gaze_pupilPre
  # globals <- config$definitions$eye$global
  # header = "6. Cleanup and export ep.eye"
  #
  log_chunk_header(header)

  ### 6.1 Generate event-locked timing
  tryCatch.ep({
    ep.eye <- ep.eye_tag_event_time(ep.eye)
  }, describe_text = "- 6.1 time_ev column generated:")

  ### 6.2 Remove raw data to cut the size of returned object considerably.
  if(globals$return_raw){
    tryCatch.ep({
        ep.eye$raw <- NULL
    }, describe_text = "- 6.2 Removing raw data:")
  } else{
    cat("6.2. Removing raw data: SKIP")
  }

  ### 6.3 Tag with ep.eye.preproc class
  # to signal that preprocessing finished without issue (though make sure to check .elog for missteps along the way)
  tryCatch.ep({
    class(ep.eye) <- c(class(ep.eye), "ep.eye.preproc")
  }, describe_text = "- 6.3 Tag with ep.eye.preproc class")

  ### 6.4 Save preprocessed data
  if(globals$save_preproc){
    tryCatch.ep({
        spath <- ep.eye_save_preproc(ep.eye,
                                     prefix = globals$prefix,
                                     out_dir = globals$preproc_out)
    }, describe_text = paste0("- 6.4 Saving preprocessed data: saved to '", spath, "':"))
  } else{
    cat("6.4 Saving preprocessed data: SKIP\n")
  }

  ### 6.5 close .elog
  if(globals$log){
  cat("- 6.5 Closing .elog: ep.eye preprocessing complete!\n--------------")
  sink(); #sink()
  }

  return(ep.eye)
}
