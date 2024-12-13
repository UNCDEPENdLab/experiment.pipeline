% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_peaks.R
\name{shift_peaks}
\alias{shift_peaks}
\title{Helper function for correct_peaks. Return the shifted peaks to local
  maxima or minima within a radius.}
\usage{
shift_peaks(sig, peak_inds, search_radius = 20, peak_up = TRUE)
}
\arguments{
\item{sig}{The 1d signal vector}

\item{peak_inds}{vector of the original peak indices}

\item{search_radius}{The integer radius within which the original peaks may be shifted.}

\item{peak_up}{: Whether the expected peak direction is up. TRUE/FALSE}
}
\value{
shifted_peak_inds vector of the corrected peak indices.
}
\description{
Helper function for correct_peaks. Return the shifted peaks to local
  maxima or minima within a radius.
}
