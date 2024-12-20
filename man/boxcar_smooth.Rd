% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_peaks.R
\name{boxcar_smooth}
\alias{boxcar_smooth}
\title{Apply a moving average filter to a time series by convolution}
\usage{
boxcar_smooth(sig, window_size = 10)
}
\arguments{
\item{sig}{The signal vector to be smoothed}

\item{window_size}{The width of a boxcar used for an equally weighted window}
}
\value{
csig The smoothed time series
}
\description{
Apply a moving average filter to a time series by convolution
}
\details{
N.B. This function mirrors the results of numpy boxcar convolution with mode='same'.
  Thus, we zero pad the series, convolve, then take the middle segment to maintain
  the same length as the original series.

  Note that this works as expected, but is remarkably slow on long time series when
  compared to zoo::rollmean or data.table::frollmean.
}
