#' function for reading multimodal data files for a subject according to a set of parsing routines
#' @importFrom checkmate assert_subset assert_file_exists
#' @export
import_subject <- function(files, yaml_file) {
  sapply(files, assert_file_exists)
  if (length(files) == 0L) { stop("No files passed to import_subject") }
  stopifnot(identical(sort(names(files)), sort(names(parsers)))) #every file needs a parser

  fnames <- names(files)
  valid_fields <- c("behav", "physio", "eye")
  assert_subset(fnames, valid_fields) #verify that event names are a subset of valid names

  # better to include all parsing information in a single YAML configuration file per task, that gets passed to subordinate functions for specific validation/QA checks per data modality.
  config <- validate_exp_yaml(yaml_file = yaml_file)

  if ("behav" %in% fnames) {
    behav <- read_behav(files["behav"], config)
  } else { behav <- NULL }

  if ("physio" %in% fnames) {
    physio <- read_physio(files["physio"], config)
  } else { physio <- NULL }

  if ("eye" %in% fnames) {
    eye <- read_process_eye(files["eye"], config)
  } else { eye <- NULL }

  ret <- list(behav=behav, physio=physio, eye=eye)
  class(ret) <- c("ep.subject", "list")
  return(ret)
}

#For physiology data, these are collected in one long acquisition, not one file per experiment.
#Thus, it makes sense to import the entire file, then to pass it to parsing functions for each experiment
#to extract relevant segments and signals.

#example
# neighborhood_sub070 <- import_subject(
#   files=c(behav="~/github_repos/experiment.pipeline/inst/examples/neighborhood/070_neighborhood_behav.csv",
#           eye="~/github_repos/experiment.pipeline/inst/examples/neighborhood/070_neighborhood_eye.edf"),
#   yaml_file = "~/github_repos/experiment.pipeline/inst/examples/yaml_config/neighborhood.yaml"
# )

