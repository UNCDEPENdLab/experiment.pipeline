############################
##### List of subsidiary functions utilized in `ep.eye_parse_events()`
############################
# - read_acq()
# - augment_ttl_details()
# - splice_physio()
# - biopac_hdf5_to_dataframe() - dont know where this is used
# - downsample_physio()
# - ep.phys_setup_structure()
# - ep.phys_get_task_length()
# - ep.phys_meta_checks()
# - ep.phys_validate_ttl_codes()
# -- ep.phys_ttl_frequency_check()
# -- ep.phys_ttl_sequence_check()
############################



### Acqknowledge read functions
#' @title Import Acqknowledge ACQ files into data.frames
#' @details
#'   This function calls the acq2hdf5 wrapper R function. The converted HDF5 files are then
#'   parsed by biopac_hdf5_to_dataframe, which relies heavily on the data.table package to build the
#'   data.frame containing physiological data.
#'
#' @return a list of
#' @param acq_files character vector of acq files to be converted to HDF5, then imported as data.frame
#' @param hdf5_output_dir location for converted hdf5 files produced by acq2hdf5. Passed to acq2hdf5.
#' @param keep_hdf5 logical indicating whether to retain hdf5 file after import completes. Default: TRUE
#' @param acq2hdf5_location path to acq2hdf5 binary, installed by the bioread python package.
#'
#' @importFrom checkmate assert_file_exists
#' @export
#' @examples
#' \dontrun{
#'    acq_data <- read_acq("~/Data_Analysis/neuromap/s4_behav_data/physio/data/nmap016/nmap016.acq",
#'                  hdf5_output_dir = "~/temp_acq",
#'                  acq2hdf5_location = "~/Library/Python/3.7/bin/acq2hdf5")
#' }
read_acq <- function(acq_files, hdf5_output_dir=NULL, keep_hdf5=TRUE, acq2hdf5_location=NULL, ...) {
  sapply(acq_files, assert_file_exists)
  if (!keep_hdf5) {
    hdf5_output_dir <- tempdir()  #output txts to temporary directory
  } else {
    if (!is.null(hdf5_output_dir) && !dir.exists(hdf5_output_dir)) { dir.create(hdf5_output_dir) } #create output directory for TXT files if requested
  }

  hdf5_files <- acq2hdf5(acq_files, acq2hdf5_location=acq2hdf5_location)

  #pass additional arguments such as parse_all to read.asc
  res <- lapply(1:length(hdf5_files), function(ff) {
    physio_data <- list(raw=biopac_hdf5_to_dataframe(hdf5file=hdf5_files[ff], ...), hdf5_file=hdf5_files[ff], acq_file=acq_files[ff])
    physio_data$sampling_rate <- attr(physio_data$raw, "sampling_rate") #promote to metadata
    physio_data$max_channel_rate <- attr(physio_data$raw, "max_channel_rate") #promote to metadata
    class(physio_data) <- c("list", "ep.physio") #add ep.physio class
    return(physio_data)
  })

  if (!keep_hdf5) { file.remove(hdf5_files) } #cleanup hdf5 files if requested

  names(res) <- basename(acq_files)
  return(res)

}


#' @title Recode TTL code vector to reflect changes in the value, compute onsets and offsets, add to object
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
#' @importFrom dplyr first group_by mutate
#' @importFrom magrittr `%>%`
#' @importFrom tidyr pivot_wider
#' @importFrom checkmate assert_count assert_data_frame
#' @author Michael Hallquist
#' @export
augment_ttl_details <- function(ep.physio, lazy_ttl=2, zero_code=0, code_labels_df=NULL) {
  stopifnot(inherits(ep.physio, "ep.physio"))
  if (is.null(ep.physio$raw)) { stop("Cannot find $raw element in ep.physio object") }
  assert_count(lazy_ttl)
  assert_count(zero_code)
  if (!is.null(code_labels_df)) {
    assert_data_frame(code_labels_df)
    stopifnot("ttl_code" %in% names(code_labels_df))
    if (any(duplicated(code_labels_df$ttl_code))) { stop("code_labels_df must contain unique ttl_codes only; no duplicates.") }
  }

  ttl_vec <- ep.physio$raw$ttl_code

  #to make comparison math below integer-valued (no bizarre x != y floating point problems)
  if (!is.integer(ttl_vec)) { ttl_vec <- as.integer(ttl_vec) }

  #if there is a stuck pin, start by replacing its occurrences by zero
  if (zero_code > 0) { ttl_vec[ttl_vec==zero_code] <- 0L }
  dvec <- which(c(NA_integer_, diff(ttl_vec)) != 0) #value changes; always treat first element as undefined
  if (length(dvec) == 0){ # if no non-zero ttl-codes present in the data
    warning("No TTL codes found in the data")
    ep.physio$ttl_codes <- NULL
    return(ep.physio)
  }
  if (lazy_ttl > 0) {
    code_diff <- c(NA_integer_, diff(dvec)) #look for cases where there are rapid changes in the difference time series
    suspicious <- dvec[which(code_diff < lazy_ttl)] #positions at which the super-fast new code is registered
    if (length(suspicious > 0)) { #window around each code
      #note that it is the *previous* code (i.e., suspicious-1) that contains the initial change
      intended_codes <- sapply(suspicious, function(pos) {
        window <- ttl_vec[(pos-2):min(length(ttl_vec), pos+10)] #from two-back position to 10 samples into the future

        if (window[1] == 0L && window[2] != 0L && window[3] == 0L) {
          #not suspicious, just an instantaneous code
          return(window[2])
        } else {
          #compute mode: tidy version of https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
          mode_ttl <- table(window) %>% sort(decreasing=TRUE) %>% names() %>% as.numeric() %>% first()
          return(mode_ttl) #use the mode of the future to determine the intention
        }
      })

      #now replace the super-brief false code with the intended code
      message("Replaced ", length(suspicious), " super-brief suspicous codes.")
      print(data.frame(position=suspicious, original_code=ttl_vec[suspicious-1], corrected_code=intended_codes))

      ttl_vec[suspicious-1] <- intended_codes

      #recalculate the ttl_change vector on the corrected codes
      dvec <- which(c(NA_integer_, diff(ttl_vec)) != 0) #value changes; always treat first element as a change
    }
  }

  delta <- rep(0L, length(ttl_vec))

  #dvec contains a mixture of onsets and offsets
  #a sequence like 0, 0, 6, 6 will generate a positive diff: 0, 0, 6, 0 for the *onset*
  #a sequence like 6, 6, 0, 0 will generate a negative diff: 0, 0, -6, 0 for the *offset*
  #one is tempted to say that positive diffs are onsets and negative diffs are offsets
  #but if there is a rapid code change like: 127, 127, 62, 62, the negative diff represents onset of a different code (no zero period)
  #to be sure, we need to window around each change to classify it
  recode_ttl <- lapply(dvec, function(pos) {
    if (pos == 1L) {
      window <- c(0L, ttl_vec[pos:(pos+1)]) #pad a zero value at the 0th position
    } else if (pos == length(ttl_vec)) {
      window <- c(ttl_vec[(pos-1):pos], 0L) #pad a zero value at the last+1 position
    } else {
      window <- ttl_vec[(pos-1):(pos+1)] #+/-1 around position
    }

    if (window[1] == 0L && window[2] != 0L && window[3] == window[2]) {
      df <- data.frame(event="onset", ttl_code=window[2], position=pos, stringsAsFactors=FALSE)
    } else if (window[2] == 0L && window[1] != 0L && window[3] == 0L) {
      df <- data.frame(event="offset", ttl_code=window[1], position=pos-1, stringsAsFactors=FALSE)
    } else if (window[1]  != window[2] && window[1] != 0L && window[2] != 0L) {
      df <- rbind(
        data.frame(event="offset", ttl_code=window[1], position=pos-1, stringsAsFactors=FALSE),
        data.frame(event="onset", ttl_code=window[2], position=pos, stringsAsFactors=FALSE)
      )
    } else if (window[1] == 0L && window[2] != 0L && window[3] == 0L) {
      #instantaneous/one-sample event: offset will be captured by second if-else above in next iteration of lapply over dvec
      df <- data.frame(event="onset", ttl_code=window[2], position=pos, stringsAsFactors=FALSE)
    } else { print(window); stop("Haven't figured out how to interpret this change in TTLs") }

    return(df)
  })

  recode_ttl <- bind_rows(recode_ttl)
  recode_ttl_df <- recode_ttl %>% group_by(ttl_code, event) %>% arrange(position) %>% mutate(occurrence=1:n()) %>%
    ungroup() %>% pivot_wider(values_from="position", names_from="event") %>%
    mutate(onset_s=ep.physio$raw$time_s[onset], offset_s=ep.physio$raw$time_s[offset], duration_s=offset_s-onset_s) %>%
    select(ttl_code, occurrence, onset, offset, onset_s, offset_s, duration_s, everything())

  #merge code details, if available
  if (!is.null(code_labels_df)) { recode_ttl_df <- recode_ttl_df %>% left_join(code_labels_df, by="ttl_code") }

  delta[dvec] <- ttl_vec[dvec]

  ep.physio$raw$ttl_onset <- delta
  ep.physio$ttl_codes <- recode_ttl_df
  return(ep.physio)
}


#' Function to splice an ep.physio object to a section demarcated by start and end codes.
#' @param ep.physio An ep.physio object that already contains the $ttl_codes field
#' @param start_code The onset TTL code that should be used for the start of splicing
#' @param end_code The offset TTL code that should be used to end the splice
#' @param other_codes an optional vector of other codes that are allowed between
#'   \code{start_code} and \code{end_code}.
#' @param strict a boolean indicating whether the function should throw an error if TTL codes
#'   are found within the splice that are not contained in \code{other_codes}
#' @importFrom checkmate assert_integerish assert_logical
#' @importFrom dplyr filter arrange
#' @importFrom magrittr `%>%`
#' @examples
#' \dontrun{
#'    #acq_data is an object read in by read_acq and then augmented by
#'    acq_data <- read_acq("~/Downloads/s3_ALL_data_prototype/070_SK_Physio.acq",
#'      acq2hdf5_location = "~/Library/Python/3.7/bin/acq2hdf5")[[1]]
#'
#'    #read in a codes data.frame into codes
#'    acq_data <- augment_ttl_details(acq_data, zero_code = 4, code_labels_df=codes)
#'
#'    #look for physio data between codes 6 and 28, allowing 12, 14, 20, 22 in between
#'    acq_data_reduce <- splice_physio(acq_data, start_code=6, end_code=28, other_codes=c(12, 14, 20, 22))
#' }
#' @export
splice_physio <- function(ep.physio, start_code=NULL, end_code=NULL, other_codes=NULL, strict=TRUE) {
  stopifnot(inherits(ep.physio, "ep.physio"))
  if (is.null(ep.physio$ttl_codes)) { stop("Cannot find $ttl_codes element in ep.physio object. Run augment_ttl_details?") }
  if (is.null(ep.physio$raw)) { stop("Cannot find $raw element in ep.physio object") }
  assert_logical(strict)
  sapply(other_codes, assert_integerish, null.ok=TRUE)
  assert_integerish(start_code)
  assert_integerish(end_code)
  code_set <- c(start_code, end_code, other_codes)

  #allow for multiple matches of start and end codes
  codes_df <- ep.physio$ttl_codes %>% arrange(onset) %>% mutate(splice_match=ttl_code %in% code_set)
  #%>% filter(ttl_code %in% code_set) %>% arrange(onset)

  if (sum(codes_df$splice_match) == 0L)  {
    warning("Unable to find any ttl_codes in the set: ", paste(code_set, collapse=", "))
    return(NULL)
  }

  #loop over ttl_codes, finding contiguous blocks of codes that fall between start_code and end_code
  block <- 0
  codes_df$block <- 0
  cur_block_code <- 0
  for (i in 1:nrow(codes_df)) {
    this_code <- codes_df$ttl_code[i]
    if (this_code == start_code) {
      block <- block+1
      cur_block_code <- block
    }

    codes_df$block[i] <- cur_block_code

    if (!is.null(other_codes) && strict && cur_block_code != 0 &&
        this_code != 0L && !this_code %in% code_set) {
      stop("TTL code ", this_code, " within block does not match acceptable set: ", paste(code_set, collapse=", "))
    }

    if (this_code == end_code) { cur_block_code <- 0 } #set block back to 0
  }

  if (max(codes_df$block)==0L) {
    warning("Unable to extract any blocks")
    return(NULL)
  } else {
    extracted <- lapply(1:block, function(b) {
      res <- codes_df %>% filter(block==b) %>% slice(1, n()) %>% summarize(onset=min(onset), offset=max(offset)) %>% unlist()
      raw_subset <- ep.physio$raw[res[1]:res[2],]
      ttl_subset <- ep.physio$ttl_codes %>% filter(onset >= res[1] & offset <= res[2])
      return(list(raw=raw_subset, ttl_codes=ttl_subset, hdf5_file=ep.physio$hdf5_file, acq_file=ep.physio$acq_file))
    })

    names(extracted) <- paste0("block", 1:block)
  }
  
  # TODO temporary changing the ep.physio structure to remove "block1". Need to discussed further to figure how the structure should look for multiple spliced blocks
  if(block == 1){
    ep.physio$raw <- extracted$block1$raw
    ep.physio$ttl_codes <- extracted$block1$ttl_codes
    return(ep.physio)
  } else {
    return(extracted)
  }
  
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

    ttl_values <- as.integer(apply(all_data[, ttl_columns, with=FALSE], 1, function(x) { bin2dec(x) }))

    #this is much much slower than apply...
    #all_data[, newcol := bin2dec(.SD), .SDcols=ttl_columns, by = seq_len(NROW(all_data))]

    all_data[, (ttl_columns) := NULL] #remove the original binary columns
    all_data[, ttl_code := ttl_values] #add ttl decimal code
  }

  attr(all_data, "sampling_rate") <- sampling_rate
  attr(all_data, "max_channel_rate") <- max_rate

  return(all_data)
}


#' @title downsample_physio
#' @description This function reduces the
#' @param ep.physio An ep.physio object created by read_acq
#' @param downsample_factor An integer factor used to subsample data
#' @param digital_channels Column names or positions containing digital channels, These will be downsampled using the
#'   \code{downsample_digital_timeseries} function to use the within-chunk mode, rather than blind subsampling.
#' @param method How to downsample the signal. The default is \code{"subsample"}, which
#'   simply takes every nth sample from the original time series. The alternative is \code{"decimate"}, which calls \code{signal::decimate}.
#'   This applies a low-pass filter before downsampling to avoid aliasing. At the moment, there are big ringing artifacts at the beginning...
#' @importFrom signal decimate
#' @importFrom checkmate assert_count assert_data_frame assert_data_table
#' @export
downsample_physio <- function(ep.physio, downsample_factor=1, digital_channels=c("ttl_code", "ttl_onset", "Digital.*"), method="subsample") {
  stopifnot(inherits(ep.physio, "ep.physio"))
  if (is.null(ep.physio$ttl_codes)) { stop("Cannot find $ttl_codes element in ep.physio object. Run augment_ttl_details?") }
  if (is.null(ep.physio$raw)) { stop("Cannot find $raw element in ep.physio object") }
  assert_data_table(ep.physio$raw) #for now, we are using data.table objects, so DT syntax applies
  assert_count(downsample_factor)

  orig_cols <- names(ep.physio$raw)
  t_cols <- "time_s" #hard code single time column for now
  phys_cols <- orig_cols[!orig_cols %in% t_cols]

  d_cols <- grep(paste0("^(", paste(digital_channels, collapse="|"), ")$"), phys_cols, perl=TRUE, value=TRUE)
  a_cols <- phys_cols[!phys_cols %in% d_cols]

  #time downsampling should use the subsampling approach since it is not a periodic signal
  if (length(t_cols) > 0L) {
    time_data <- lapply(ep.physio$raw[, ..t_cols], function(col) { col[seq(1, length(col), downsample_factor)] })
  } else {
    time_data <- NULL
  }

  if (length(a_cols) > 0L) {
    if (method=="decimate") {
      analog_data <- lapply(ep.physio$raw[, ..a_cols], function(col) {
        #calculation channels that involve filtering can have trailing zeros that throw off decimate
        nas = which(is.na(col))
        if (length(nas) > 0L) {
          if (all(diff(nas) == 1) && max(nas) == length(col)) { #only works for trailing NAs
            message("Replacing ", length(nas), " trailing NAs with zeros to permit decimation. Check trailing elements if they are important.")
            col[nas] <- 0 #replace trailing NAs with zero
            #col <- col[1:(nas[1]-1)] #all elements before the first NA
            #navec <- rep(NA_real_, ceiling(length(col)/downsample_factor))
          } else { stop("NAs present in signal that are not at the end. Cannot decimate.") }
        }

        padl <- plyr::round_any(.1*length(col), downsample_factor)
        cpad <- c(rep(0, padl), col, rep(0, padl))
        nz_dsamp <- padl/downsample_factor
        dsig <- decimate(cpad, q=downsample_factor, ftype="iir")
        return(dsig[(nz_dsamp+1):(length(dsig)-nz_dsamp)])
      })
    } else if (method=="subsample") {
      analog_data <- lapply(ep.physio$raw[, ..a_cols], function(col) { col[seq(1, length(col), downsample_factor)] })
    } else { stop("unknown downsampling method: ", method) }
  } else {
    analog_data <- NULL
  }

  if (length(d_cols) > 0L) {
    #could support subsampling here -- doesn't seem like a great idea, though
    digital_data <- lapply(ep.physio$raw[, ..d_cols], function(col) { downsample_digital_timeseries(col, downsample_factor, TRUE) })
  } else {
    digital_data <- NULL
  }

  if (is.null(analog_data)) {
    ret <- digital_data
  } else if (is.null(digital_data)) {
    ret <- analog_data
  } else {
    ret <- cbind(as.data.frame(analog_data), as.data.frame(digital_data))
    ret <- ret[,phys_cols] #revert to original column order
  }

  if (!is.null(time_data)) { ret <- cbind(ret, as.data.frame(time_data)) }

  ep.physio$raw <- ret[,orig_cols] #put back in original column order
  attr(ret, "sampling_rate") <- attr(ret, "sampling_rate")/downsample_factor
  attr(ret, "max_channel_rate") <- attr(ret, "max_channel_rate")/downsample_factor
  ep.physio$sampling_rate <- ep.physio$sampling_rate/downsample_factor
  ep.physio$max_channel_rate <- ep.physio$max_channel_rate/downsample_factor

  #downsample onsets in $ttl_codes
  if (!is.null(ep.physio$ttl_codes)) {
    ep.physio$ttl_codes$onset <- floor(ep.physio$ttl_codes$onset/downsample_factor) #round toward earlier samples
    ep.physio$ttl_codes$offset <- floor(ep.physio$ttl_codes$offset/downsample_factor) #round toward earlier samples
  }

  return(ep.physio)

}




#' @title Initialize a ep.phys data structure
#' @description Initialize an empty ep.phys data structure which will get filled-in with actual data as the data passed through different functions of the pipeline. 
#' @param physio_data loaded .acq file from \code{read_acq}
#' @param config_ecg configuration for ecg data in list format
#' @param config_eda configuration for eda data in list format
#' @param task name of the task for which the data is processed
#' 
#' @importFrom data.table data.table
#' @return ep.physio returns a list of 5 objects of type lists or data.tables. These objects are raw data, ecg processed data and qa, 
#'   eda processed data and qa, ttl codes and metadata.
#' 
#' @author Nidhi Desai
#' 
#' @export
#' @examples TODO add examples
ep.phys_setup_data_structure <- function(physio_data, 
                                         config_ecg,
                                         config_eda,
                                         task = NULL){
  ep.physio <- list(raw = physio_data$raw,
                  ecg = list(processed = data.table(),
                             qa = list()),
                  eda = list(scr = data.table(),
                             event_scr = data.table(),
                             scl = data.table(),
                             qa = list()),
                  ttl_codes = data.table(),
                  metadata = list(sampling_rate = physio_data$sampling_rate,
                                  max_channel_rate = physio_data$max_channel_rate,
                                  acq_file = physio_data$acq_file,
                                  hdf5_file = physio_data$hdf5_file,
                                  ecg = list(config = config_ecg),
                                  eda = list(config = config_eda)) # TODO format in setup config function from input config file
  )
  # tag with ep.physio class
  class(ep.physio) <- c(class(ep.physio), "ep.physio")
  
  # rename EDA and ECG signal name in ep.physio$raw
  for (x in c("ecg", "eda")){
    raw_indx <- which(grepl(x, tolower(names(ep.physio$raw)), fixed = TRUE))
    if(length(raw_indx) == 0){
      ep.physio[[x]] <- NULL
      ep.physio[["metadata"]][[x]] <- NULL
    } else {
      # add the signal name to metadata
      ep.physio$metadata[[x]][["signalname"]] <- names(ep.physio$raw)[raw_indx]
      # rename the ecg signal in raw
      names(ep.physio$raw)[raw_indx] <- x
    }
  }

  # append task name to metadata
  if(!is.null(task)) ep.physio[["metadata"]][["task"]] <- task
  return(ep.physio)
}


#' @title Calculates the recording length
#' @description This function calculates the recording length of the session. If the raw data has been spliced for a task, then this will calculate the task length.
#' @param ep.physio An initialized ep.physio object. See \code{ep.phys_setup_structure} to initialize this object.
#' 
#' @return ep.physio object with recording_time added to \code{ep.physio$metadata}
#' 
#' @author Nidhi Desai
#' 
#' @export
ep.phys_get_recording_length <- function(ep.physio){
  # recording time for the task in seconds
  ep.physio$metadata$recording_time <- max(ep.physio$raw$time_s) - min(ep.physio$raw$time_s)
  return(ep.physio)
}


#' @title Checks on metadata 
#' @description This function perform checks on metadata like comparing actual values of session parameters (ex: sampling data) with expected values mentioned in config file. 
#'   Also checking if task recording time in physio data lies within the expectated range. Reference ep.eye_meta_check() functions for reference. # examples of metachecks: sample rate, max_channel_rate, recording length.
#' @param ep.physio An initialized ep.physio object. See \code{ep.phys_setup_structure} to initialize this object.
#' @param meta_vars Character vector of meta variables to check
#' @param meta_vals Character vector of values to expect for the meta variables passed above
#' @param recording_time Numeric vector of length 2 indicating the expected recording session time *in seconds* and the margin of error above and below the expected recording time without generating an error.
#' 
#' @return ep.physio object with \code{ep.physio$metadata$meta_check} added.
#' 
#' @author Nidhi Desai
#' 
#' @export
ep.phys_meta_checks <- function(ep.physio, 
                                meta_vars,
                                meta_vals,
                                recording_time){
  
  ### 2.5.1 meta_vars and vals
  stopifnot(!(is.null(meta_vars) | is.null(meta_vals))) # stop if either are null, need both.
  stopifnot(length(meta_vars) == length(meta_vals))
  
  meta_ref <- data.frame(meta_vars, meta_vals) %>% mutate_all(as.character)
  
  mismatch_var <- c() # append if any discrepancies.
  mismatch_expected <- c()
  mismatch_actual <- c()
  for(i in 1:nrow(meta_ref)){
    # message(eye$metadata[[meta_ref[i,"meta_vars"]]], " ", meta_ref[i,"meta_vals"])
    if(!ep.physio$metadata[[meta_ref[i,"meta_vars"]]] == meta_ref[i,"meta_vals"]){
      mismatch_var <- c(mismatch, meta_ref[i,"meta_vars"])
      mismatch_expected <- c(mismatch_expected, meta_ref[i,"meta_vals"])
      mismatch_actual <- c(mismatch_actual, ep.physio$metadata[[meta_ref[i,"meta_vars"]]])
    }
  }
  
  if(!is.null(mismatch_var)){
    warning("meta variables not match expected value: ", mismatch_var, call. = FALSE)
    ep.physio[["metadata"]][["meta_check"]][["meta_vars"]][["meta_vars_mismatch"]] <- mismatch_var
    ep.physio[["metadata"]][["meta_check"]][["meta_vars"]][["meta_vars_expected"]] <- mismatch_expected
    ep.physio[["metadata"]][["meta_check"]][["meta_vars"]][["meta_vars_actual"]] <- mismatch_actual
  } else {
    ep.physio[["metadata"]][["meta_check"]][["meta_vars"]] <- NULL
  }
  
  ### 2.5.2 confirm acceptable session length
  stopifnot(!is.null(recording_time))
  stopifnot(!is.null(ep.physio$metadata$recording_time))
  
  rt_range <- c(recording_time[1] - recording_time[2],
                recording_time[1] + recording_time[2])
  
  if(!all((rt_range[1] <= ep.physio$metadata$recording_time) & (ep.physio$metadata$recording_time <= rt_range[2]))){
    warning("recording length (", ep.physio$metadata$recording_time,") outside of expected bounds: ", paste0(rt_range, collapse = ", "), call. = FALSE)
    ep.physio[["metadata"]][["meta_check"]][["recording_length_violation"]] <- TRUE
    # TODO should we add the expected recording length here?
  } else{
    ep.physio[["metadata"]][["meta_check"]][["recording_length_violation"]] <- FALSE
  }
}



#' @title Perform validation checks on the parport codes frequency and sequence
#' @description 
#' @param ep.physio An initialized ep.physio data object.
#' @param ttl_codes_freq tibble with ttl information with each row contains information regarding one ttl code that could be sent with the physio data and the columns contain the ttl_code, the stimuli and phase that this ttl_code will be sent in and expected frequency of this ttl_code 
#' @author Nidhi Desai
#' 
#' @export
ep.phys_validate_ttl_codes <- function(ep.physio, ttl_codes_freq){
  
  ep.physio <- ep.phys_ttl_frequency_check(ep.physio, ttl_codes_freq)
  
  
  ep.physio <- ep.phys_ttl_sequence_check(ep.physio)
  
  return(ep.physio)
}

#' @title check the frequency of ttl codes
#' @description Checks the actual frequency for each ttl_code with the expected frequency calculated in \code{ep.phys_build_ttl_seq}
#' @param ep.physio An initialized ep.physio data object.
#' @param ttl_codes_freq tibble with ttl information with each row contains information regarding one ttl code that could be sent with the physio data and the columns contain the ttl_code, the stimuli and phase that this ttl_code will be sent in and expected frequency of this ttl_code 
#' 
#' @import dplyr filter
#' 
#' @return ep.physio data structure with metadata$ttl_checks$frequency_mismatch containing list of ttl codes, expected frequency and actual frequency for the ttl codes whose actual frequency does not match the expected frequency calculated using config file in \code{ep.phys_build_ttl_seq}. ep.physio$metadata$ttl_blocks_info contains a tibble with the ttl_code, corresponsding stimuli, phase, expected and actual frequency.
#'
#' @author Nidhi Desai, Nila Thillaivanan
#' 
#' @export
ep.phys_ttl_frequency_check <- function(ep.physio, ttl_codes_freq){

  actual_freq =  rep(NA, nrow(ttl_codes_freq))
  for(i in c(1:nrow(ttl_codes_freq))){
    actual_freq[i] <- nrow((ep.physio$ttl_codes %>% filter(ttl_code == ttl_codes_freq$ttl_code[i])))
    
    if (actual_freq[i] != ttl_codes_freq$expected_freq[i]){
      mismatch_ttl <- ttl_codes_freq$ttl_code[i]
      mismatch_expected <- ttl_codes_freq$expected_freq[i]
      mismatch_actual <- actual_freq[i]
      
      ep.physio[["metadata"]][["ttl_checks"]][["frequency_mismatch"]][["tt_codes"]] <- mismatch_ttl
      ep.physio[["metadata"]][["ttl_checks"]][["frequency_mismatch"]][["expected_frequency"]] <- mismatch_expected
      ep.physio[["metadata"]][["ttl_checks"]][["frequency_mismatch"]][["actual_frequency"]] <- mismatch_actual
      
      warning(paste0("[frequency mismatch] ttl code: ", mismatch_ttl, ", expected:", mismatch_expected, ", actual:", mismatch_actual))
    }
  }
  
  ttl_blocks_info <- ttl_codes_freq %>% add_column(actual_freq)
  ep.physio[["metadata"]][["ttl_codes_info"]][["freq"]] <- ttl_codes_freq
  
  return(ep.physio)
}


#' @title check the sequence of ttl codes
#' @description check if the ttl codes are in the correct sequence based on the blocks section in config file
#' @param ep.physio An initialized ep.physio data object
#' 
#' @import 
#' 
#' @return
#' 
#' @author Nidhi Desai
#' 
#' @export
ep.phys_ttl_sequence_check <- function(ep.physio){
  # TODO left here
  # ep.physio[["metadata"]][["ttl_codes_info"]][["seq"]] ## need to add the check result here
  
}









