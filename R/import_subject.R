#' function for reading multimodal data files for a subject according to a set of parsing routines
#' @importFrom checkmate assert_subset assert_file_exists
#' @export
import_subject <- function(files, parsers) {
  sapply(files, assert_file_exists)
  if (length(files) == 0L) { stop("No files passed to import_subject") }
  stopifnot(identical(sort(names(files)), sort(names(parsers)))) #every file needs a parser

  fnames <- names(files)
  valid_fields <- c("behav", "physio", "eye")
  assert_subset(fnames, valid_fields) #verify that event names are a subset of valid names

  if ("behav" %in% fnames) {
    behav <- read_behav(files["behav"], parser=parsers$behav)
  } else { behav <- NULL }

  if ("physio" %in% fnames) {
    physio <- read_physio(files["physio"], parser=parsers$physio)
  } else { physio <- NULL }

  if ("eye" %in% fnames) {
    eye <- read_eye(files["eye"], parser=parsers$eye)
  } else { eye <- NULL }

  return(list(behav=behav, physio=physio, eye=eye))
}

#For physiology data, these are collected in one long acquisition, not one file per experiment.
#Thus, it makes sense to import the entire file, then to pass it to parsing functions for each experiment
#to extract relevant segments and signals.

#example
# abc <- import_subject(
#   files=c(behav="~/Box/s3_behav_data/neighborhood/behavior/data/raw/003_Cavanaugh_2019_Sep_30_1720.csv",
#           eye="~/Box/s3_behav_data/neighborhood/eye/data/raw/N_003_TS.edf"),
#   parsers=list(behav=read_behav_neighborhood, eye=read_eye_neighborhood)
# )

