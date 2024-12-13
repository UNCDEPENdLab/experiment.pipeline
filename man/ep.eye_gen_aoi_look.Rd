% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ep.eye_preprocess_helpers.R
\name{ep.eye_gen_aoi_look}
\alias{ep.eye_gen_aoi_look}
\title{Attaches AOI information to gaze data}
\usage{
ep.eye_gen_aoi_look(ep.eye, aoi_ref, tag_raw = FALSE, dt = NULL)
}
\arguments{
\item{ep.eye}{an ep.eye object}

\item{aoi_ref}{data.frame containing AOI information}

\item{tag_raw}{logical. tag raw data with AOI specific information? Default to FALSE.}

\item{dt}{descriptive text to print to log file, defaults to NULL.}
}
\description{
Attaches AOI information to gaze data
}
