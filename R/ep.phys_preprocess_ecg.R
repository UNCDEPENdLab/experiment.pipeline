#' @title Process ecg data through ECG pre-processing#'
#' @param splice_data physio data will need to be spliced to extract the task relevant data
#' 
#' @author Nidhi Desai
#' 
ep.phys_preprocess_ecg <- function(ep.physio,
                                   detect_beats = TRUE,
                                   wfdb_path = "/proj/mnhallqlab/sw/wfdb-10.6.2/bin",
                                   detectors = c("wqrs", "gqrs", "sqrs"),
                                   ibi_qa = TRUE,
                                   ibi_outliers = "ibi_diff >  upper_quartile_ibi_diff + 2*iqr_ibi_diff | ibi_diff < lower_quartile_ibi_diff - 2*iqr_ibi_diff"){
  # Run QRS beat detection
  
  ecg_detect_beats(ecg_trace = ep.physio$raw$ecg,
                   freq = ep.physio$metadata$sampling_rate,
                   wfdb_path = wfdb_path, 
                   detectors = detector,
                   correct_peaks = TRUE)
  
  
  ibi_qa(ecg_ts = ep.physio$raw$ecg, 
         time_ts = ep.physio$raw$time_s, 
         Hz = ep.physio$metadata$sampling_rate,
         outlier_arg = ibi_outliers)
  
  
}