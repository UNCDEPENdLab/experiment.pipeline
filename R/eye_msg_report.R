
eye_msg_report <- function(eye, report_path = NULL, events = "all", return_eye = FALSE){
  #for a quick look at ET message flow by event. Allows for user to quickly look for messaging conventions that can be specified in YAML config file.

  if(is.character(eye)){ # if string provided, attempt to read it.
    eye <- read_edf(eye, keep_asc=FALSE, parse_all=TRUE, samples = FALSE)[[1]] #samples = FALSE removes read-in of raw data
  }

  stopifnot(exists("eye")) # must be input as argument or an edf must supply it. Can be read internally, supplied as the output of read_edf, or as an already initialized ep.eye object.

  #open creation of message report.
  if(is.null(report_path)){
    sink(file = "eye_message_format.txt")
  } else{
    if(!dir.exists(str_extract(report_path, ".*/"))) dir.create(str_extract(report_path, ".*/"))
    sink(file = report_path)
  }

  if("ep.eye" %in% class(eye)){
    cat("dropping msgs with '!V IMGLOAD CENTER'\n")
    for(i in unique(eye$raw$eventn)){
        y.w <- eye$raw  %>% dplyr::filter(eventn == i & et.msg != "." & !grepl("!V IMGLOAD CENTER", et.msg))
        print(na.omit(y.w), n =Inf)
    }

    eye$metadata$btw_tr_msg %>% as_tibble() %>% dplyr::filter(eventn > 97 &eventn < 102)%>% print(n = Inf)


  } else { #works from initial edf, either read in or input as arg.
    uevents <- unique(eye$msg$eventn)
    tol <- 1e-12
    uint <- uevents[sapply(uevents, function(y) min(abs(c(y%%1, y%%1-1))) < tol)] #drop non-integers: https://stackoverflow.com/questions/30476671/selecting-only-integers-from-a-vector

    #print message format by recording event. Can select to also include between-trial messages if they are important (events = "all" will report within and between trial messages)
    if(events %in% c("all", "within")){
      for(i in uint){
        y.w <- eye$msg  %>% dplyr::filter(eventn == i)
        print(na.omit(y.w), n =Inf)
      }
    }

    if(events %in% c("all", "between")){
      unoint <- uevents[which(!uevents %in% uint)]
      for(i in unoint){
        y.b <- eye$msg  %>% dplyr::filter(eventn == i)
        print(na.omit(y.b), n =Inf)
      }
    }
    if(return_eye){return(eye)}
  }

  sink() # close report.
}
