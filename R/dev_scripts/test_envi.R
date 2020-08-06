
# generate local environment for debugging eyetracking QA -----------------

pacman::p_load(eyelinker, FDBeye, tidyverse)

basedir <- "C:/Users/vietp/Documents/PennState/DEPENd_Lab/experiment.pipeline/"

source(paste0(basedir, "/R/import_subject.R"))
source(paste0(basedir, "/R/read_eye.R"))
source(paste0(basedir, "/R/edf2asc.R"))
source(paste0(basedir, "/R/import-data.R"))


edf_path <- paste0(basedir, "inst/examples/070_neighborhood_eye.edf")


read_eye <- function(file, parser=NULL, ...) {
  
  stopifnot(file.exists(file))
  ### generic import across any experimental task or session.
  eye <- read_eye_generic(file) 
  
  if (is.null(parser)) { 
    # stop("Need to pass parser function to read_eye") 
    message("Only generic parsing applied")
  } else{
    message("Additional task parsing applied via: ", parser)
    eye <- parser(eye, ...)
  }
  
  class(eye) <- c(class(eye), "ep.eye") #tag with ep.eye class
  
  #other general post-processing for eye data
  return(eye)
}


#' specific function for importing neighborhood eye tracking data into the package
#' need to add task-specific processing
#' @export
read_eye_neighborhood <- function(file) {
  if (length(file) > 1L) { stop("At present, read_eye_neighborhood is designed for one file at a time") }
  #store as temp asc file
  eye_parsed <- read_edf(file, keep_asc=FALSE, parse_all=TRUE)[[1]] #read_edf always returns list -- here we only one the one file
  return(eye_parsed)
}



#' specific function for importing neighborhood eye tracking data into the package
#' need to add task-specific processing
#' @export
read_eye_generic <- function(file) {
  if (length(file) > 1L) { stop("At present, read_eye_generic is designed for one file at a time") }
  ### 1. Read EDF file into environment
  #store as temp asc file
  eye_parsed <- read_edf(file, keep_asc=FALSE, parse_all=TRUE)[[1]] #read_edf always returns list -- here we only one the one file
  ### 2. make sure all names are present
  expected_edf_fields <- c("raw", "sacc", "fix", "blinks", "msg", "input", "info")
  stopifnot(all(expected_edf_fields %in% names(eye_parsed)))
  ### 3. make sure all timestamps are present
  
  
  
  return(eye_parsed)
}



this.sub.eye <- read_eye(edf_path, read_eye_neighborhood)



# for(s in subjects){
#   for(t in tasks){
#     specific_path <- paste0((s,t))
#     this.subject <- import_subject(specific_path)
#   }
# }
