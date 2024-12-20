% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ep.eye_initialize_helpers.R
\name{ep.eye_store_between_event_messages}
\alias{ep.eye_store_between_event_messages}
\title{Extract messages that are passed between recording events.}
\usage{
ep.eye_store_between_event_messages(ep.eye)
}
\arguments{
\item{ep.eye}{An ep.eye object.}
}
\value{
ep.eye Returns the same ep.eye object, with between-event messages stored in metadata.
}
\description{
Some task/quality-relevant messages may (depending on how the experiment is setup) be passed to the .edf file in between times where the tracker is actively recording data (e.g. prior to a screen flip or during calibration and validation). This function extracts such messages in ep.eye[["metadata"]]. These "between event messages", are denoted with a eventn ending in .5. For example, messages passed in "eventn" 1.5 are passed between recording events 1 and 2 and may contain information about the previous or following event.
}
