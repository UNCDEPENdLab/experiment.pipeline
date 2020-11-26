#' Initializes .elog file which will store eye-specific messages about how QA checks are going.
#'
#'  Initially, my thought is to create .blog and .plog files for behavior and physio respectively and eventually write a separate function (or set of functions) that searches through logs and documents how certain checks went in a more succinct format (e.g. a .csv with subjects on rows and stages of QA on columns).
#'  A report can then be generated to either verify folks that look fine or to draw attention to problematic data.
#'

init_eyelog <- function(log_dir = NULL){

  if(is.null(log_dir)){log_dir <- getwd(); message("Generating .elog file in current directory: ", getwd())} else{
    message("Generating .elog file in: ", log_dir)
  }


  fname <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", file) # strips ascending path and replaces file extension
  log_fname <- file.path(log_dir, paste0(fname, ".elog"))

  if(file.exists(log_fname)){
    message("elog already exists for: ", fname, ". Overwriting.") #can entertain other options, but this is fine for now.
  }

  sink(log_fname) # open sink

  cat("---------------------------------------------\n---------------------------------------------\nexperiment.pipeline eye QA log for: ", file,
      "\n\nProcessed on ")
  cat(as.character(Sys.time()))
  cat("\n---------------------------------------------\n---------------------------------------------\nsessionInfo:\n\n")
  print(sessionInfo())
  cat("\n---------------------------------------------\n---------------------------------------------\n")

}



#' TryCatch function to run subroutines with ease and standardized output.
#'
#' @param code chunk of code for tryCatch to evaluate. As in the TC documentation, multiple lines of code should be contained within {}.
#' @param describe_text string containing standardized information about the significance of the code being run. This will print as COMPLETE for successful execution and ERROR and WARNING is something undesirable happens, while allowing code execution to continue below.

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
  if(!any(c("error", "warning") %in% class(o))) {cat(describe_text, " COMPLETE\n", sep = "")}
}


#' Open log chunks (to be run at the top of every sub-function of read_process_eye)
#' simply standardizes for later crawler scripts to recognize

log_chunk_header <- function(text){cat("--------------\n",text,"\n--------------\n")}


#' simple increment function for steps: https://stackoverflow.com/questions/5738831/r-plus-equals-and-plus-plus-equivalent-from-c-c-java-etc
# inc <- function(x){eval.parent(substitute(x <- x + 1))}





#' generate warning message version of stopifnot
#'
#' @importFrom R.utils egsub
#'

# this was a fail. for some reason the trycatch exports an error.

# warnifnot <- function(cond){
#   if(!cond){warning(cond, "is not TRUE")}
# }
#
# warnifnot <- stopifnot
# body(warnifnot) <- do.call(substitute, list(body(stopifnot),
#                                             list(stop = quote(warning))))
