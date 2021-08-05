#' @title Validate experimental config file
#' @param yaml_file path to YAML config file (see vignette on how to set up your ep.eye config file).
#' @importFrom yaml read_yaml
#' @importFrom checkmate assert_int assert_count assert_subset
#' @author Michael Hallquist
#' @export
validate_exp_yaml <- function(yaml_file) {
  stopifnot(file.exists(yaml_file))
  yy <- read_yaml(yaml_file)

  reqnames <- c("task", "variable_mapping",
                "runs", "blocks")

  #NH: is this required to validate the yaml?
  optnames <- c("definitions") #aliases for reused snippets/nodes.


  # stopifnot(all(reqnames %in% names(yy)))
  assert_subset(reqnames, names(yy))

  return(yy)
}

#' @importFrom checkmate assert_int assert_count assert_subset
validate_exp_block <- function(block_node) {
  block_names <- names(block_node)

  valid_fields <- c("parport_code", "trials", "events", "behav")
  assert_subset(block_names, valid_fields) #verify that block_names are a subset of valid names
  # if (length(ss <- setdiff(block_names, valid_fields)) > 0L) {
  #   stop("block contains the following unrecognized fields: ", paste(ss, collapse=", "))
  # }

  #trials is a required field
  stopifnot("trials" %in% names(block_names))
  assert_count(block_node$trials, positive = TRUE) #verify that trials is positive integer

  #verify that block parport is integer between 0 and 255 (NULL is okay to denote no parport)
  assert_int(block_node$parport_code, lower = 0, upper = 255, null.ok=TRUE)

  #validate all event nodes
  sapply(block_node$events, validate_exp_events)
}

#' @importFrom checkmate assert_subset
validate_exp_events <- function(event_node) {
  event_names <- names(event_node)

  valid_fields <- c("parport_code", "eye_msg")
  assert_subset(event_names, valid_fields) #verify that event names are a subset of valid names

}

test_block <- function(subject, block_node) {
  #subject will have $behav, $eye, $physio

}

