
eye_msg_report <- function(file, report_path = NULL, blocks = "within", return_eye = FALSE){
  #for a quick look at ET message flow by block. Allows for user to quickly look for messaging conventions that can be specified in YAML config file.
  eye <- read_edf(file, keep_asc=FALSE, parse_all=TRUE, samples = FALSE)[[1]] #samples = FALSE removes read-in of raw data

  ublocks <- unique(eye$msg$block)
  tol <- 1e-12
  uint <- ublocks[sapply(ublocks, function(y) min(abs(c(y%%1, y%%1-1))) < tol)] #drop non-integers: https://stackoverflow.com/questions/30476671/selecting-only-integers-from-a-vector

  #open creation of message report.
  if(is.null(report_path)){
    sink(file = "eye_message_format.txt")
  } else{
    sink(file = report_path)
  }

  #print message format by recording block. Can select to also include between-trial messages if they are important (blocks = "all" will report within and between trial messages)
  if(blocks %in% c("all", "within")){
    for(i in uint){
      y <- eye$msg  %>% dplyr::filter(block == i)
      print(na.omit(y))
    }
  }

  if(blocks %in% c("all", "between")){
    unoint <- ublocks[which(!ublocks %in% uint)]
    for(i in unoint){
      y <- eye$msg  %>% dplyr::filter(block == i)
      print(na.omit(y))
    }
  }
  sink() # close report.

  if(return_eye){return(eye)}
}
