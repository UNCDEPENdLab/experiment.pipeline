ep.eye_cleanup <- function(ep.eye,
                           globals,
                           header = NULL){

  ### debug 
  ep.eye <- eye_gaze_pupilPre
  globals <- config$definitions$eye$global
  header = "6. Cleanup and export ep.eye"
  
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
    class(eye) <- c(class(eye), "ep.eye.preproc")
  }, describe_text = "- 6.3 Tag with ep.eye.preproc class")
  
  ### 6.4 Save preprocessed data
  if(globals$save_preproc){
    tryCatch.ep({
        spath <- ep.eye_save_preproc(ep.eye,
                                     prefix = globals$prefix,
                                     out_dir = globals$preproc_out)
    }, describe_text = paste0("- 6.4 Saving preprocessed data: saved to '", spath, "':"))  
  } else{
    cat("6.4 Saving preprocessed data: SKIP")
  }
  
  ### 6.5 close .elog
  cat("- 6.5 Closing .elog: ep.eye preprocessing complete!\n--------------")
  sink(); #sink()
  
  return(ep.eye)
}
