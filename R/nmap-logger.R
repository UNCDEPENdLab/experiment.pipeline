# Logging Wrapper Script
#
# Wrapper for the logging library for specific usage for NeuroMap.
# The usage is identical except we keep track of a single logger to
# allow for a more dynamic logger.

#' Creates a log object that outputs to the console.
#'
#' @param name Name of the logger.
#' @param level Level for logger to operate at.
#'
#' @return NULL
create.log <- function(name, level = 20) {
  logging::setLevel(level, name)
}

#' Logs message as an info using log object
#'
#' @param msg Message to log
#' @param logger Logger object name
log.info <- function(msg, logger = '') {
  logging::loginfo(msg, logger = logger)
}

#' Logs message as a warning using log object
#'
#' @param msg Message to log
#' @param logger Logger object name
log.warn <- function(msg, logger = '') {
  logging::logwarn(msg, logger = logger)
}

#' Logs message as a error using log object
#'
#' @param msg Message to log
#' @param logger Logger object name
log.error <- function(msg, logger = '') {
  logging::logerror(msg, logger = logger)
}

#' Logs message as a debug using log object
#'
#' @param msg Message to log
#' @param logger Logger object name
log.debug <- function(msg, logger = '') {
  logging::logdebug(msg, logger = logger)
}
