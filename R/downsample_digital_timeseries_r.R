#' here's a pure R version of downsampling a categorical/digital timeseries
#' @param x input digital (categorical) timeseries vector
#' @param downsamp downsampling factor (integer > 1)
#' @param demote_zeros If TRUE, don't allow zero to be the retained value within a downsampled chunk
#' @export
downsample_digital_timeseries_r <- function(x, downsamp, demote_zeros=TRUE) {
  checkmate::assert_flag(demote_zeros)
  checkmate::assert_integerish(x)
  checkmate::assert_integerish(downsamp, len=1L, lower=1)

  n <- length(x)

  nchunks <- ceiling(n/downsamp) # number of chunks

  mode_int <- function(x) {
    # if you want this to match C++ exactly, you need to sort ux, but this slows things down a lot
    # and only ensures that in cases where there are multiple modes, the lowest mode wins (arbitrary)
    # ux <- sort(unique(x))
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
  }

  ds <- sapply(seq_len(nchunks), function(i) {
    nrem <- min(n, i*downsamp)
    idx <- (1 + (i-1)*downsamp):nrem
    this_chunk <- x[idx]
    if (demote_zeros && !all(this_chunk == 0L)) this_chunk <- this_chunk[!this_chunk == 0L]
    mode_int(this_chunk)
  })

  return(ds)
}
