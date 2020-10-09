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
      
  # cat("hello")
  # sink()
}
