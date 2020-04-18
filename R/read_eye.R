#' general wrapper for reading eye data into the package
read_eye <- function(file, parser=NULL, ...) {
  if (is.null(parser)) { stop("Need to pass parser function to read_eye") }
  stopifnot(file.exists(file))
  eye <- parser(file, ...)
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
