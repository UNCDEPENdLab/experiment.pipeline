% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ep.eye_parse_events_helpers.R
\name{ep.eye_validate_msg_seq}
\alias{ep.eye_validate_msg_seq}
\title{Validate the presence of task-general messages by event}
\usage{
ep.eye_validate_msg_seq(ep.eye, msg_seq, dt)
}
\arguments{
\item{ep.eye}{ep.eye object that has been previously initialized and tidied}

\item{msg_seq}{List of optional message sequence arguments passed in config file. Can contain \code{msg_start}, \code{msg_end}, \code{eval_middle}, \code{ordered}. See the ep.eye_config vignette for details on these fields.}

\item{dt}{descriptive text to print}
}
\value{
ep.eye
}
\description{
Validate the presence of task-general messages by event
}
\author{
Nate Hall
}
