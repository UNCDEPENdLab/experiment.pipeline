#' @title Recode TTL code vector to reflect changes in the value
#' @description This functions helps to identify the key time markers when TTL codes onset,
#'   which is hard to detect using the raw vector alone (since the code will have some duration)
#' @param ttl_vec a vector of integer TTL codes reconstructed from the acq file
#' @param lazy_ttl a non-negative integer indicating the lowest number of samples that could
#'   reflect a genuine change in a TTL code. Default: 2
#' @param zero_code what code reflects that all pins are off (no active code). If you have a stuck
#'   parallel port pin, set \code{zero_code} to that value to prevent flagging onsets of that value
#'   as events.
#'
#' @details
#'   The parallel port cannot truly change all 8 pins simultaneously for a new code.
#'   If the speed of the pins changing falls on a sampling boundary on the BIOPAC hardware, this can lead
#'   to false codes for very brief periods of time (~2ms). Thus, identify rapid code changes, then
#'   interrogate around these, replacing the dubious code with the code that 'sticks' subsequently.
#'   This decision means that if any pin has changed, then the experiment has executed a new TTL code.
#'
#'   For example, at 1000Hz, @\code{lazy_ttl} = 2 would flag any codes with duration of 2ms or less as
#'   suspicious and would replace these with the longer, genuine code that immediately follows.
#' @importFrom dplyr first
#' @importFrom magrittr `%>%`
#' @author Michael Hallquist
compute_ttl_onsets <- function(ttl_vec, lazy_ttl=2, zero_code=0) {
  assert_count(lazy_ttl)
  assert_count(zero_code)

  #if there is a stuck pin, start by replacing its occurrences by zero
  if (zero_code > 0) { ttl_vec[ttl_vec==zero_code] <- 0 }
  dvec <- which(c(1, diff(ttl_vec)) != 0) #value changes; always treat first element as a change

  if (lazy_ttl > 0) {
    code_diff <- c(NA_integer_, diff(dvec)) #look for cases where there are rapid changes in < 10ms/samples?
    suspicious <- dvec[which(code_diff < lazy_ttl)] #positions at which the super-fast new code is registered
    if (length(suspicious > 0)) { #window around each code
      #note that it is the *previous* code (i.e., suspicious-1) that contains the initial change
      intended_codes <- sapply(suspicious, function(pos) {
        window <- ttl_vec[(pos-1):(pos+10)] #from previous position to 10 samples into the future

        #compute mode: tidy version of https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
        mode_ttl <- table(window) %>% sort(decreasing=TRUE) %>% names() %>% as.numeric() %>% first()
        return(mode_ttl) #use the mode of the future to determine the intention
      })

      #now replace the super-brief false code with the intended code
      message("Replaced ", length(suspicious), " super-brief suspicous codes.")
      print(data.frame(position=suspicious, original_code=ttl_vec[suspicious-1], corrected_code=intended_codes))

      ttl_vec[suspicious-1] <- intended_codes

      #recalculate the ttl_change vector on the corrected codes
      dvec <- which(c(1, diff(ttl_vec)) != 0) #value changes; always treat first element as a change
    }
  }

  delta <- rep(0, length(ttl_vec))
  delta[dvec] <- ttl_vec[dvec]
  return(delta)
}


#' @title Read a BIOPAC hdf5 file as a data.frame
#' @description After converting an acq file using acq2hdf5, this function converts the hdf5 file to a data.frame
#' @details
#'   This function uses the data.table package to build the data.frame of physiological data because this package
#'   is particularly good at working with large datasets without a lot of copy-in-memory steps that can slow down
#'   processing and explode the peak RAM demand.
#'
#'   The function also supports upsampling of all data onto the same time resolution as the fastest
#'   sampling rate for any channel. This is governed by the \code{upsample_to_max} argument
#' @param hdf5file HDF5 file containing converted BIOPAC data (using acq2hdf5)
#' @param upsample_to_max logical (TRUE/FALSE) indicating whether to upsample slower channels to the sampling
#'   rate of the fastest channel. If \code{TRUE}, upsampling is performed on slower channels through linear interpolation.
#'   If \code{FALSE}, NA values are inserted for unsampled time points.
#' @param ttl_to_dec logical indicating whether to convert binary digital input channels to decimal TTL (parallel port)
#'   codes. These codes are used to synchronize timing between behavioral and physiological data streams.
#' @param ttl_columns A numeric vector or character vector indicating which columns within the imported dataset
#'   contain the digital inputs that should be converted to decimal TTL codes. If not specified, channels
#'   starting with Digital.input will be used. There should be exactly 8 channels for a 1-byte (0-255) TTL code.
#' @importFrom rhdf5 h5readAttributes h5dump h5read
#' @importFrom checkmate assert_file_exists assert_scalar assert_logical assert_list
#' @importFrom data.table data.table setkey setnames
#' @export
biopac_hdf5_to_dataframe <- function(hdf5file, upsample_to_max=TRUE, ttl_to_dec=TRUE, ttl_columns=NULL) {
  assert_scalar(hdf5file) #only support one input
  assert_logical(upsample_to_max)
  assert_logical(ttl_to_dec)
  assert_file_exists(hdf5file)

  #helper subfunction to convert a vector of binary fields to a decimal number
  bin2dec <- function(vec) { sum((vec > 0) * 2^(seq_along(vec) - 1)) } #use vec > 0 to convert digital inputs to 0/1 (instead of 5 volts)

  #BIOPAC overall sampling rate (not used in time grid)
  sampling_rate <- rhdf5::h5readAttributes(hdf5file, "/")$samples_per_second

  #determine number of channels
  hdf5contents <- h5dump(hdf5file, load=FALSE)
  assert_list(hdf5contents$channels) #verify that we have a list of channels
  channel_names <- names(hdf5contents$channels)
  channel_attributes <- lapply(channel_names, function(x) { h5readAttributes(hdf5file, paste0("/channels/", x)) })
  channel_rates <- sapply(channel_attributes, "[[", "samples_per_second")
  channel_data <- h5read(hdf5file, "/channels")
  channel_names <- make.names(sapply(channel_attributes, "[[", "name"), unique = TRUE)
  max_len <- max(sapply(channel_data, length))
  max_rate <- max(channel_rates)
  deltat <- 1/max_rate

  all_data <- data.table::data.table(time_s=seq(0, by=deltat, length.out=max_len))
  setkey(all_data, time_s)

  #loop over channels and append them as columns to overall data.table
  for (cc in 1:length(channel_data)) {
    rr <- channel_rates[cc]
    dd <- 1/rr
    cname <- channel_names[cc]
    this_channel <- data.table(time_s = seq(0, by=dd, length.out = length(channel_data[[cc]])), data = channel_data[[cc]])
    if (upsample_to_max) {
      interp <- approx(this_channel$time_s, this_channel$data, xout=all_data$time_s)$y #linear interpolation
      this_channel <- data.table(time_s = all_data$time_s, data = interp)
    }
    setnames(this_channel, c("time_s", cname))
    setkey(this_channel, time_s)
    all_data <- merge(all_data, this_channel, by="time_s", all.x=TRUE)
  }

  if (ttl_to_dec) {
    if (is.null(ttl_columns)) {
      ttl_columns <- startsWith(names(all_data), "Digital.input")
    } else if (is.character(ttl_columns)) {
      ttl_columns <- grepl(ttl_columns, names(all_data), perl = TRUE)
    }

    ttl_columns <- names(all_data)[ttl_columns] #data.table prefers names for subsetting
    if (length(ttl_columns) != 8) { stop("ttl_columns does not yield 8 columns in data frame")}

    ttl_values <- apply(all_data[, ttl_columns, with=FALSE], 1, function(x) { bin2dec(x) })

    #this is much much slower than apply...
    #all_data[, newcol := bin2dec(.SD), .SDcols=ttl_columns, by = seq_len(NROW(all_data))]

    all_data[, (ttl_columns) := NULL] #remove the original binary columns
    all_data[, ttl_code := ttl_values] #add ttl decimal code
  }

  return(all_data)
}
