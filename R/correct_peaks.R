#this file contains R ports of the wfdb-python package peaks.py source
#these functions are used for shifting detected peaks from QRS detectors
#https://github.com/MIT-LCP/wfdb-python/

#' Adjust a set of detected peaks to coincide with local signal maxima.
#'
#' @param sig The 1d signal vector
#' @param peak_inds Vector of the original peak indices
#' @param search_radius The radius within which the original peaks may be shifted.
#' @param smooth_window_size The window size of the moving average filter applied on the
#'          signal. Peak distance is calculated on the difference between
#'          the original and smoothed signal.
#' @param peak_dir optional string denoting the expected peak direction:
#'          'up' or 'down', 'both', or 'compare'.
#' @details
#'   Functionality of \code{peak_dir} param:
#'        - If 'up', the peaks will be shifted to local maxima.
#'        - If 'down', the peaks will be shifted to local minima.
#'        - If 'both', the peaks will be shifted to local maxima of the
#'          rectified signal.
#'        - If 'compare', the function will try both 'up' and 'down'
#'          options, and choose the direction that gives the largest mean
#'          distance from the smoothed signal.
#'
#' @return shifted_peak_inds vector of the corrected peak indices.
#' @export
correct_peaks <- function(sig, peak_inds, search_radius, smooth_window_size, peak_dir='compare') {
  sig_len = length(sig)
  n_peaks = length(peak_inds)

  # Subtract the running average smoothed signal from the original

  # Use frollmean in data.table, which is super-fast
  #zero pad the series to avoid NAs/0s at the head and tail
  #this is closest to the original pad and convolve approach from numpy
  sigavg <- c(rep(0, smooth_window_size), sig, rep(0, smooth_window_size))
  sigavg <- data.table::frollmean(sigavg, smooth_window_size, fill=NA, align="center", algo="fast")

  #trim off the head and tail that were padded
  sig <- sig - sigavg[(smooth_window_size+1):(smooth_window_size + length(sig))]
  rm(sigavg) #cleanup since these may be big time series

  #system.time(sig2 <- zoo::rollmean(sig, smooth_window_size, align="center", fill=0))
  #sig3 <- data.table::frollmean(sig, smooth_window_size, fill=NA, align="center", algo="fast")
  #sig4 <- boxcar_smooth(sig=sig, window_size=smooth_window_size)

  #sig = sig - boxcar_smooth(sig=sig, window_size=smooth_window_size)

  # Shift peaks to local maxima
  if (peak_dir == 'up') {
    shifted_peak_inds = shift_peaks(
      sig=sig,
      peak_inds=peak_inds,
      search_radius=search_radius,
      peak_up=TRUE)
  } else if (peak_dir == 'down') {
    shifted_peak_inds = shift_peaks(
      sig=sig,
      peak_inds=peak_inds,
      search_radius=search_radius,
      peak_up=FALSE)
  } else if (peak_dir == 'both') {
    shifted_peak_inds = shift_peaks(
      sig=abs(sig),
      peak_inds=peak_inds,
      search_radius=search_radius,
      peak_up=TRUE)
  } else { #'compare'
    shifted_peak_inds_up = shift_peaks(
      sig=sig,
      peak_inds=peak_inds,
      search_radius=search_radius,
      peak_up=TRUE)

    shifted_peak_inds_down = shift_peaks(
      sig=sig,
      peak_inds=peak_inds,
      search_radius=search_radius,
      peak_up=FALSE)

    # Choose the direction with the biggest deviation
    up_dist = mean(abs(sig[shifted_peak_inds_up]))
    down_dist = mean(abs(sig[shifted_peak_inds_down]))

    if (up_dist >= down_dist) {
      shifted_peak_inds = shifted_peak_inds_up
    } else {
      shifted_peak_inds = shifted_peak_inds_down
    }
  }

  return(shifted_peak_inds)

}


#' Helper function for correct_peaks. Return the shifted peaks to local
#'   maxima or minima within a radius.
#'
#' @param sig The 1d signal vector
#' @param peak_inds vector of the original peak indices
#' @param search_radius The integer radius within which the original peaks may be shifted.
#' @param peak_up : Whether the expected peak direction is up. TRUE/FALSE
#'
#' @return shifted_peak_inds vector of the corrected peak indices.
#' @importFrom checkmate assert_vector
shift_peaks <- function(sig, peak_inds, search_radius=20, peak_up=TRUE) {
  checkmate::assert_vector(sig)
  checkmate::assert_vector(peak_inds)

  sig_len <- length(sig)
  n_peaks <- length(peak_inds)

  # The indices to shift each peak ind by
  shift_inds <- rep(0L, n_peaks)

  # Iterate through peaks
  for (i in 1:n_peaks) {
    #cat("On peak ", i, " of ", n_peaks, "\n")
    ind = peak_inds[i]
    local_sig = sig[max(1, ind - search_radius):min(ind + search_radius, sig_len)]
    if (isTRUE(peak_up)) {
      shift_inds[i] <- which.max(local_sig)
    } else {
      shift_inds[i] = which.min(local_sig)
    }
  }

  # Adjust early values to pad the max/min position left-truncated in the window
  for (i in 1:n_peaks) {
    ind = peak_inds[i]

    #only execute for indices smaller where windowing around search_radius produced negative values
    if (ind >= search_radius) { break }

    #N.B. This is mistake in the wfdb-python code base. We want to *add* the
    #search_radius - ind (as if we had no left truncation), not subtract it.
    shift_inds[i] <- shift_inds[i] + (search_radius - ind)
  }

  #shift to 1-based indexing in R, so when shift_inds - 1 == search_radius, the resulting shift is zero.
  #shifted_peak_inds = peak_inds + (shift_inds - 1) - search_radius
  
  #on further testing, I think the -1 is likely wrong since it can lead to indices of 0 (when which.max is the first sample)
  shifted_peak_inds = peak_inds + shift_inds - search_radius

  return(shifted_peak_inds)
}

#' Apply a moving average filter to a time series by convolution
#'
#' @param sig The signal vector to be smoothed
#' @param window_size The width of a boxcar used for an equally weighted window
#'
#' @details
#'   N.B. This function mirrors the results of numpy boxcar convolution with mode='same'.
#'   Thus, we zero pad the series, convolve, then take the middle segment to maintain
#'   the same length as the original series.
#'
#'   Note that this works as expected, but is remarkably slow on long time series when
#'   compared to zoo::rollmean or data.table::frollmean.
#'
#' @return csig The smoothed time series
#' @importFrom stats convolve
boxcar_smooth <- function(sig, window_size=10) {
  box <- rep(1, window_size)/window_size
  csig <- convolve(sig, box, type = "open")
  firstobs <- ceiling(window_size/2)
  lastobs <- length(csig) - floor(window_size/2)
  return(csig[firstobs:lastobs])
}

#' Helper function to read single-channel WFDB annotations into a data.table object
#'
#' @param wfdb_file Prefix to wfdb file containing the annotations to be extracted
#' @param annotator The algorithm used for creating the annotations
#'   (determines the suffix, like 100.gqrs)
#' @param elapsed A boolean indicating whether to extract elapsed times since beginning of record
#'   or absolute times. A value of TRUE passes -e to rdann
#' @param channel optional channel to read annotations from if file contains multi-channel data.
#'   Default is 0, which is the first channel.
#' @param type The type of annotation to be imported from WFDB. Defaults to "N" for normal beats in ECG
#' @param wfdb_path location of the WFDB bin directory on this computer.
#'
#' @importFrom data.table fifelse fread
#' @importFrom checkmate assert_file_exists assert_logical assert_integerish assert_directory_exists assert_file_exists
#' @importFrom lubridate hms
#' @author Michael Hallquist
#' @export
import_wfdb_annotations <- function(wfdb_file, annotator, elapsed=TRUE, channel=0L, type="N",
                                    wfdb_path="/usr/local/wfdb/bin") {

  checkmate::assert_file_exists(paste(wfdb_file, annotator, sep="."))
  checkmate::assert_logical(elapsed)
  checkmate::assert_integerish(channel)
  checkmate::assert_directory_exists(wfdb_path)
  checkmate::assert_file_exists(file.path(wfdb_path, "rdann"), access="rx") #does rdann exist and is executable

  #wfdb does not play well with absolute paths... needs to be in current path
  anntmp <- tempfile()
  curwd <- getwd()
  setwd(dirname(wfdb_file))
  on.exit(curwd)
  eflag <- ifelse(isTRUE(elapsed), " -e ", "")

  system(paste0(wfdb_path, "/rdann -r ", basename(wfdb_file), " -a ", annotator, " -p ", type, " -c ", channel, eflag, " > ", anntmp))
  anndf <- data.table::fread(anntmp)
  setnames(anndf, c("time", "pos", "ann_type", "ann_subtype", "chan", "num"))

  # hms in lubridate doesn't do well if we leave off the hours field from any of the records to convert, such as
  #    00:01.51. It will think that the period denotes the minutes-seconds boundary. We need to pad those records with
  #    "00:" to have the conversion work as expected.
  has_hours <- grepl("\\d+:\\d+:\\d+.*", anndf$time, perl=TRUE)
  anndf <- anndf[, time := fifelse(has_hours, time, paste0("00:", time)) ]

  anndf[, time_sec := as.numeric(lubridate::hms(anndf$time), "seconds")] #lubridate conversion to seconds
  return(anndf)
}
