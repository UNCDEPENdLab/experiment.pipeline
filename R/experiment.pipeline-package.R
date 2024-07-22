#' @title R package for processing multi-modal experiment data
#' @docType package
#' @name experiment.pipeline
#'
#' @description This package provides utilities for reading data from experiments
#'   that involve recording of behavior, peripheral physiology, and eye tracking.
#'   The goal is to support a pipeline that can import, validate, and synchronize
#'   these data streams with each other.
#'
#' @details
#'
#' \tabular{ll}{
#' Package: \tab experiment.pipeline\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1-1\cr
#' Date: \tab 2020-04-20\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' LazyData: \tab yes\cr
#' }
#'
#' @author
#' Michael Hallquist \email{michael.hallquist@@gmail.com},
#' Nate Hall \email{nate.hall329@gmail.com},
#' Eric Roeum \email{evr5285@@psu.edu}
#'
#' Maintainer: Michael Hallquist \email{michael.hallquist@@gmail.com}
#' @keywords package
#' @useDynLib experiment.pipeline, .registration = TRUE
#' @importFrom utils packageDescription
#' @importFrom Rcpp sourceCpp
"_PACKAGE"
