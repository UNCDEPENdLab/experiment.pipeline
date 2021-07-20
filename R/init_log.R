#' @title Initializes .elog
#' @description .elog files uses \code{sink()} to create a .txt file which will store eye-specific messages about how QA checks are going.
#' @param file Path to the .edf file to process.
#' @param log_dir Path to directory to store .elog. If NULL (default) will write to current directory.
#' @param prefix Optional prefix to append to .elog name. 
#' @return Nested list with processing options pulled from .yml configuration file.
#' @note Initially, my thought is to create .blog and .plog files for behavior and physio respectively and eventually write a separate function (or set of functions) that searches through logs and documents how certain checks went in a more succinct format (e.g. a .csv with subjects on rows and stages of QA on columns).
#'  A report can then be generated to either verify folks that look fine or to draw attention to problematic data.
#' @author Nate Hall
init_eyelog <- function(file, log_dir = NULL, prefix = NULL){

  if(is.null(log_dir)){log_dir <- getwd(); message("Generating .elog file in current directory: ", getwd())} else{
    message("Generating .elog file in: ", log_dir)
    if(!dir.exists(log_dir)){
      dir.create(log_dir, recursive = TRUE)
    }
  }

  if(is.null(prefix)) prefix <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", file) # strips ascending path and replaces file extension
  log_fname <- file.path(log_dir, paste0(prefix, ".elog"))

  if(file.exists(log_fname)){
    message("elog already exists for: ", log_fname, ". Overwriting.") #can entertain other options, but this is fine for now.
  }

  sink(log_fname) # open sink

  cat("---------------------------------------------\n---------------------------------------------\nexperiment.pipeline eye QA log for: ", file,
      "\n\nProcessed on ")
  cat(as.character(Sys.time()))
  cat("\n---------------------------------------------\n---------------------------------------------\nsessionInfo:\n\n")
  print(sessionInfo())
  cat("\n---------------------------------------------\n---------------------------------------------\n")

}



#' @title TryCatch function to run subroutines with ease and standardized output.
#' @description This tryCatch wrapper prints errors and warnings alongside information describing the process being executed. If code runs succesfully will print COMPLETE. These errors/warnings are ideally passed to a log file that documents sequential steps in a pipeline. 
#' @param code Chunk of code for tryCatch to evaluate. As in the TC documentation, multiple lines of code should be contained within curly brackets[{}].
#' @param describe_text String containing standardized information about the significance of the code being run. This will print as COMPLETE for successful execution and ERROR and WARNING is something undesirable happens, while allowing code execution to continue below.

tryCatch.ep <- function(code, describe_text = NULL){
    o <- tryCatch(code,
                  error = function(c) {
                    if(is.null(describe_text)){
                      cat("ERROR (", gsub("Error: ", "",gsub("\\n", "", as.character(c))),")\n", sep = "")
                    } else {cat(describe_text, " ERROR (", gsub("Error: ", "",gsub("\\n", "", as.character(c))),")\n", sep = "")}
                    return(c)},
                  warning = function(c) {
                    if(is.null(describe_text)){
                      cat("WARNING (", gsub("simpleWarning: ", "",gsub("\\n", "", as.character(c))),")\n", sep = "")
                    } else {cat(describe_text, " WARNING (", gsub("simpleWarning: ", "",gsub("\\n", "", as.character(c))),")\n", sep = "")}
                    return(c)}
    )
  # print complete if no error or warning
  if(!any(c("error", "warning") %in% class(o)) & !is.null(describe_text)) {cat(describe_text, " COMPLETE\n", sep = "")}
}


#' @description  Prints log chunks (to be run at the top of every sub-function of read_process_eye)
#' @note This simply standardizes for later crawler scripts to recognize.
#' @param text Text to print into chunk header.

log_chunk_header <- function(text){cat("--------------\n",text,"\n--------------\n")}