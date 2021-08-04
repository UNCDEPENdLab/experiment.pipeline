
ep.eye_preprocess_gaze <- function(ep.eye, 
                                   aoi,
                                   downsample,
                                   header = NULL){
 
  log_chunk_header(header)

  ### 4.1 Remove impossible values (outside of screen dimensions)
  dt <- "- 4.1 Remove impossible values (outside of screen dimensions):"
  ep.eye <- ep.eye_rm_impossible(ep.eye, dt)

  ### 4.2 Assign AOIs
  if (!is.null(aoi)) {
    cat("- 4.2 Assign AOIs to gaze data\n")
    ep.eye <- ep.eye_add_aois(ep.eye, 
                       indicator = aoi$indicator,
                       extract_coords = aoi$extract_coords,
                       extract_labs = aoi$extract_labs,
                       split_coords = aoi$split_coords,
                       tag_raw = aoi$tag_raw)
  } else{
    cat("- 4.2 Assign AOIs to gaze data: SKIP")
  }

  ### 4.3 Collapse timestamps
  dt <- "- 4.3 Collapse timestamps with more than one row:"
  ep.eye <- ep.eye_collapse_time(ep.eye, dt)


  ### N.B. 3/8/21 Interpolating eye data, even in a simplistic manner, is a bad idea given that gaze data is very non-smooth. Better to just rely on the good readings and remove trials with too much data missing, rather than trying to save missing samples.
  ### If the user disagrees the function is still available to use on the preprocessed data, but this should not be included in the standard pipeline.
  # ### 4.4 Interpolate
  # if (!"interpolate" %in% names(c.gaze)) {
  #   cat("- 4.4 Interpolate gaze data: SKIP")
  # } else{
  #   dt <- "- 4.4 Interpolate gaze data:"
  #   tryCatch.ep({
  #     eye <- interpolate_eye(eye, config, signal = "gaze")
  #   },describe_text = dt)
  # }

  ### 4.4 Downsample gaze
  if(!is.null(downsample)){
    tryCatch.ep({
      ep.eye$gaze$downsample <- ep.eye_downsample(ep.eye$raw,
                                                  downsample_factor = downsample$factor,
                                                  analog_channels = c("xp", "yp"),
                                                  method = downsample$method)
    }, describe_text = "- 4.4 Downsample gaze:")
  }

  return(ep.eye)
}